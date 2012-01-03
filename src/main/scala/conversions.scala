package ls

import com.mongodb.casbah.Imports._

object Conversions extends Logged {
  import com.mongodb.casbah.commons.Imports.ObjectId
  import com.mongodb.casbah.commons.{MongoDBObject => Obj, MongoDBList => ObjList}
  import com.mongodb.casbah.{MongoCollection, MongoCursor}
  import com.mongodb.{BasicDBList, DBObject}
  import com.mongodb.casbah.Implicits._
  import java.util.Date

  // todo complete this
  val StopWords = Seq(
    "a", "and", "as", "at", "but", "for", "the"
  )

  private def keywords(l: Library) =
    l.description.replaceAll("""\s+"""," ").toLowerCase.split(" ").map(_.trim) ++
      l.tags.map(_.trim) ++ Seq(
        l.organization, l.name, l.version,
        l.ghuser, l.ghrepo
      ).filterNot(
        StopWords.contains(_)
      )

  private def keywords(l: LibraryVersions) = 
    l.description.replaceAll("""\s+"""," ").toLowerCase.split(" ").map(_.trim) ++
      l.tags.map(_.trim) ++ Seq(
        l.organization, l.name, /*l.version, */
        l.ghuser, l.ghrepo
      ).filterNot(
        StopWords.contains(_)
      )

  implicit val libraryVersionsToDbObject: LibraryVersions => DBObject =
    (l: LibraryVersions) =>
      Obj(
      "organization" -> l.organization,
      "name" -> l.name,
      "description" -> l.description,
      "_keywords" -> keywords(l),
      "tags" -> l.tags,
      "site" -> l.site,
      "sbt" -> l.sbt,
      "ghuser" -> l.ghuser,
      "ghrepo" -> l.ghrepo,
      "updated" -> new Date().getTime,
      "contributors" ->
        l.contributors.getOrElse(Nil).map { c =>
          Obj(
            "login" -> c.login,
            "id" -> c.id,
            "avatar_url" -> c.avatar_url,
            "url" -> c.url
          )
        },
      "versions" -> versionsToDbObjects(l.versions)
    )

  def libraryDepToDbObject(m: ModuleID) =
    Obj(
      "organization" -> m.organization,
      "name" -> m.name,
      "version" -> m.version
    )

  def licenseToDbObject(l: License) = l match {
    case License(n, u) => Obj(
      "name" -> n,
      "url" -> u
    )
  }

  implicit val libraryToDbObject: Library => DBObject =
    (l: Library) =>
      Obj(
        "organization" -> l.organization,
        "name" -> l.name,
        "description" -> l.description,
        "_keywords" -> keywords(l),
        "tags" -> l.tags,
        "site" -> l.site,
        "sbt" -> l.sbt,
        "ghuser" -> l.ghuser,
        "ghrepo" -> l.ghrepo,
        "contributors" ->
          l.contributors.getOrElse(Nil).map { c =>
            Obj(
              "login" -> c.login,
              "id" -> c.id,
              "avatar_url" -> c.avatar_url,
              "url" -> c.url
            )
          },
        "updated" -> new Date().getTime,
        "versions" ->
          Seq(Obj( // this should NOT delete any exisiting versions
            "version" -> l.version,
            "docs" -> l.docs,
            "resolvers" -> l.resolvers,
            "dependencies" ->
              l.dependencies.map(libraryDepToDbObject),
            "licenses" -> l.licenses.map(licenseToDbObject),
            "scalas" -> l.scalas
          ))
      )

  private def versionsToDbObjects(versions: Seq[Version]) =
    versions.map { v =>
      Obj(
        "version" -> v.version,
        "docs" -> v.docs,
        "resolvers" -> v.resolvers,
        "dependencies" ->
          v.dependencies.map(libraryDepToDbObject),
          "licenses" -> v.licenses.map(licenseToDbObject),
          "scalas" -> v.scalas
      )
    }

  implicit val versionOrdering: Ordering[Version] = Ordering.by((_:Version).version).reverse

  //private def innerList(m: Obj)(objName: String, lname: String) =
  //  wrapDBList(wrapDBObj(m.getAs[BasicDBList](objName).get).getAs[BasicDBList](lname).get)

  def first(m: Obj)(objName: String, prop: String): Option[String] =
    wrapDBList(m.getAs[BasicDBList](objName).get).iterator match {
      case it if(it hasNext) =>
        it.next.asInstanceOf[DBObject].getAs[String](prop)
      case _ => None
    }

  private def moduleId(o: DBObject) =
    ModuleID(o.getAs[String]("organization").get,
            o.getAs[String]("name").get,
            o.getAs[String]("version").get)

  private def contributor(o: DBObject) =
    User(o.getAs[String]("login").get,
         o.getAs[Int]("id").get,
         o.getAs[String]("avatar_url").get,
         o.getAs[String]("url").get)

  private def license(o: DBObject) =
    License(o.getAs[String]("name").get,
     o.getAs[String]("url").get)

  private def dbObjectToVersion(m:Obj) = try { Version(
    m.getAs[String]("version").get,
    m.getAs[String]("docs").get,
    wrapDBList(m.getAs[BasicDBList]("resolvers").get)
      .iterator.map(_.toString).toSeq,
    wrapDBList(m.getAs[BasicDBList]("dependencies").get)
      .iterator.map(l => moduleId(wrapDBObj(l.asInstanceOf[DBObject]))).toSeq,
    wrapDBList(m.getAs[BasicDBList]("scalas").get)
      .iterator.map(_.toString).toSeq,
    m.getAs[BasicDBList]("licenses") match {
      case Some(dbl) =>
        wrapDBList(dbl).iterator.map(l => license(l.asInstanceOf[DBObject])).toSeq
      case _ => Seq()
    }
  ) } catch {
    case e =>
      log.error("failed to parse %s" format m)
      throw e
  }

  implicit val dbObjectToLibraryVersions: Obj => LibraryVersions = (m) => try { LibraryVersions(
    m.getAs[String]("organization").get,
    m.getAs[String]("name").get,
    m.getAs[String]("description").get,
    m.getAs[String]("site").get,
    wrapDBList(m.getAs[BasicDBList]("tags").get)
      .iterator.map(_.toString).toSeq,
    m.getAs[Long]("updated").getOrElse(new Date().getTime),
    wrapDBList(m.getAs[BasicDBList]("versions").get)
      .iterator.map(v => dbObjectToVersion(v.asInstanceOf[BasicDBObject])).toSeq,
    m.getAs[Boolean]("sbt").getOrElse(false),
    m.getAs[String]("ghuser"),
    m.getAs[String]("ghrepo"),
    m.getAs[BasicDBList]("contributors") match {
      case Some(dbl) =>
        Some(wrapDBList(dbl)
             .iterator.map(c => 
               contributor(wrapDBObj(c.asInstanceOf[DBObject]))
              ).toSeq)
      case _ => None
    }
  ) } catch {
    case e =>
      log.error("failed to parse %s" format m)
      throw e
  }

  implicit val mc2lv: MongoCollection => Iterable[LibraryVersions] = 
    (m) => for(l <- m) yield dbObjectToLibraryVersions(l)

  implicit val mislv: Iterator[DBObject] => Iterable[LibraryVersions] =
    (m) => (for(l <- m) yield dbObjectToLibraryVersions(l)).toSeq

  // todo: contribute back to casbah
  implicit def mdbo2optpp(dbo: DBObject) = new {
    def opt(opt: Option[DBObject]): DBObject = opt match {
      case Some(other) => dbo ++ other
      case _ => dbo
    }
  }
}
