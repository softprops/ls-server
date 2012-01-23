package ls

import com.mongodb.casbah.Imports._

// for reference http://www.mongodb.org/display/DOCS/Advanced+Queries

object Libraries extends Logged {
  import Conversions._
  import com.mongodb.casbah.commons.Imports.ObjectId
  import com.mongodb.casbah.commons.{MongoDBObject => Obj, MongoDBList => ObjList}
  import com.mongodb.casbah.{MongoCollection}
  import com.mongodb.{BasicDBList, DBObject}
  import com.mongodb.casbah.Implicits._

  type CanConvertListTo[A] = Iterator[DBObject] => Iterable[A]
  type CanConvertTo[A, B] = A => B

  val DefaultLimit = 20

  def anycase(term: String) = """(?i)%s""".format(term).r

  def narrowAnycase(term: String) = """(?i)^%s$""".format(term).r

  private def libraries[T](f: MongoCollection => T) =
    Store.collection("libraries")(f)

  /** searches for librarues by author/contributors */
  def author[T, C](ghuser: String)(f: Iterable[C] => T)
                  (implicit cct: CanConvertListTo[C]) =
    libraries { c =>
      log.info("getting libraries for author %s" format ghuser)
      f(cct(c.find(
        $or("ghuser" -> anycase(ghuser), "contributors.login" -> anycase(ghuser))
      )))
    }

  /** search by any search terms */
  def any[T, C](terms: Seq[String])
              (page: Int = 1, lim: Int = DefaultLimit)(f: Iterable[C] => T)
              (implicit cct: CanConvertListTo[C]) =
   libraries { c =>
     log.info("getting libraries for terms %s" format terms.mkString(", "))
     val possiblies =  (MongoDBObject().empty /: terms)(
       (a, e) => a += ("name" -> anycase(e))
     )
     val parts = (possiblies ++ ("_keywords" $in terms)).toMap
     val query = $or(parts.toSeq:_*)
     log.info("any query: %s" format query)
     f(cct( paginate(c.find(query), page, lim).sort(Obj("updated" -> -1)) ))
   }

  // this will always return one less and one more
  // for pagination hints
  def paginate(c: MongoCursor, page: Int, lim: Int) =
    c.skip(math.max(0, (lim * (page - 1)) - 1)).limit(lim + 1)

  /** get a pagingates list of all libraries */
  def all[T, C](page: Int = 1, lim: Int = DefaultLimit)(f: Iterable[C] => T)
               (implicit cct: CanConvertListTo[C])=
    Store.collection("libraries") { c =>
      log.info("getting libraries (page: %s, lim: %s)" format(page, lim))
      f(cct( paginate(c.find(), page, lim).sort(Obj("updated" -> -1)) ))
    }

  /** For project-based queries */
  def projects[T, C](user: String, repo: Option[String] = None)(
    f: Iterable[C] => T)(implicit cct: CanConvertListTo[C]) =
    libraries { c =>
      log.info(
        "getting libraries for user: %s, repo: %s" format(
          user, repo)
      )
      val query =
        Obj("ghuser" -> anycase(user)) opt repo.map(r => Obj("ghrepo" -> anycase(r)))
      log.info("query: %s" format query)
      f(cct(
        c.find(query)
      ))
    }

  def latest[T, C](
    name: String, user: Option[String] = None,
    repo: Option[String] = None)
    (f: Iterable[C] => T)(implicit cct: CanConvertListTo[C]) =
    libraries { c =>
      log.info("geting latest version of %s (%s/%s)" format(name, user, repo))
      val query = Obj("name" -> narrowAnycase(name)) opt user.map(u =>
        Obj("ghuser" -> narrowAnycase(u))
      ) opt repo.map(r =>
        Obj("ghrepo" -> narrowAnycase(r))
      )
      f(cct(c.find(
        query,
        Obj("_id" -> 0, "versions.version" -> 1)
      )))
    }

  /** Find by name + version and optionally user and repo */
  def apply[T, C](
    name: String,
    version: Option[String] = None,
    user: Option[String] = None,
    repo: Option[String] = None)
    (f: Iterable[C] => T)(implicit cct: CanConvertListTo[C]) =
    libraries { c =>
      log.info("getting libraries for name: %s, user: %s, repo: %s" format(
        name, user, repo
      ))
      val query =
        Obj("name" -> narrowAnycase(name)) opt user.map(u =>
          Obj("ghuser" -> narrowAnycase(u))
        ) opt repo.map(r =>
          Obj("ghrepo" -> narrowAnycase(r))
        ) opt version.map(v =>
          Obj("versions.version" -> version)
        )
      log.info("query: %s" format query)
      f(cct(
        c.find(query)
      ))
    }

  // merge/update before simply appending to collection
  // potential hammersmith candiate
  def save(libs: Seq[Library]) = libraries { col =>
    log.info("saving or updating %d libraries" format libs.size)
    libs.map { l =>
      val query = Obj(
        "name" -> l.name, "organization" -> l.organization,
        "ghuser" -> l.ghuser, "ghrepo" -> l.ghrepo
      )
      log.info("create or update selection query %s" format query)

      apply(l.name, user = l.ghuser, repo = l.ghrepo){ (currentVersions: Iterable[LibraryVersions]) =>
        if(currentVersions.isEmpty) try { col.findAndModify(
          query, // query
          Obj(), // fields
          Obj(), // sort
          false, // rm 
          libraryToDbObject(l),// update
          true,  // returned new
          true   // create or update
        ) } catch {
          case e => e.printStackTrace
        } else {
          // this could get ugly!
          val current: LibraryVersions = currentVersions.head.copy(
            description = l.description,
            site = l.site,
            tags = l.tags,
            sbt = l.sbt,
            contributors = l.contributors
          )
          val versions = current.versions.toSeq
          val (contained, notcontained) = versions.partition(_.version == l.version)
          if(contained.isEmpty) {
            val appended = (Version(
              l.version, l.docs,
              l.resolvers, l.dependencies,
              l.scalas,
              l.licenses
            ) +: versions).sorted
            val updating = libraryVersionsToDbObject(current.copy(
              versions = appended
            ))
            // append version
            col.findAndModify(
              query,
              Obj(),
              Obj(),
              false, 
              updating,
              true,
              true
            )
          } else {
            // update version
            val merged = (contained(0).copy(
              version = l.version,docs = l.docs,
              resolvers = l.resolvers, dependencies = l.dependencies,
              scalas = l.scalas
            ) +: notcontained).sorted
            val updated = libraryVersionsToDbObject(current.copy(
              versions = merged
            ))
            col.findAndModify(
              query,
              Obj(),
              Obj(),
              false, 
              updated,
              true,
              true
            )            
          }
        }
      }
    }
  }
}
