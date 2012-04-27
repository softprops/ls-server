package ls

object G8Conversions extends Logged {
  import com.mongodb.casbah.commons.Imports.ObjectId
  import com.mongodb.casbah.commons.{MongoDBObject => Obj, MongoDBList => ObjList}
  import com.mongodb.casbah.{MongoCollection, MongoCursor}
  import com.mongodb.{BasicDBList, DBObject}
  import com.mongodb.casbah.Implicits._

  private def dbObjectToTemplate(m:Obj) = try {
    G8.Template(
      m.getAs[String]("username").get,
      m.getAs[String]("name").get,
      m.getAs[String]("description").get
    )
  } catch {
    case e =>
      log.error("failed to parse %s" format m)
      throw e
  }

  implicit val mit: Iterator[DBObject] => Iterable[G8.Template] =
    (m) => (for(t <- m) yield dbObjectToTemplate(t)).toSeq
}

object G8 extends Logged {
  import Mongo._
  import com.mongodb.casbah.commons.Imports.ObjectId
  import com.mongodb.casbah.commons.{ MongoDBObject => Obj, MongoDBList => ObjList }
  import com.mongodb.casbah.{ MongoCollection }
  import com.mongodb.{ BasicDBList, DBObject }
  import com.mongodb.casbah.Implicits._

  case class Template(username: String, name: String, description: String)

  implicit val templateToDbObject: Template => DBObject =
    (t: Template) =>
      Obj(
        "username" -> t.username, // deprecated
        "name" -> t.name,
        "description" -> t.description
      )

  def DefaultLimit = 200

  private def templates[T](f: MongoCollection => T) =
    Store.collection("g8")(f)

  /** @return paginated set of all templates */
  def all[C, T](page: Int = 1, limit: Int = DefaultLimit)(f: Iterable[C] => T)(implicit cct: CanConvertListTo[C]) =
    templates { c =>
      log.info("getting templates (page: %s, limit: %s)" format(page, limit))
      f(cct(paginate(c.find(), page, limit).sort(Obj("username" -> 1, "name"-> 1))))
    }

   def apply[T, C](
    username: String,
    name: Option[String] = None)
    (f: Iterable[C] => T)(implicit cct: CanConvertListTo[C]) =
    templates { c =>
      log.info("getting templates for username: %s, name: %s" format(
        username, name
      ))
      val query =
        Obj("username" -> narrowAnycase(username)) opt name.map(n =>
          Obj("name" -> narrowAnycase(n))
        )
      log.info("query: %s" format query)
      f(cct(
        c.find(query)
      ))
    }

  def save(tmpls: Seq[Template]) = templates { col =>
    log.info("saving or updating %d templates" format tmpls.size)
    tmpls.map { t =>
      val query = Obj(
        "username" -> t.username,
        "name" -> t.name
      )
      log.info("create or update selection query %s" format query)
      col.findAndModify(
        query, // query
        Obj(), // fields
        Obj(), // sort
        false, // rm
        templateToDbObject(t),// update
        true,  // returned new
        true   // create or update
      )
    }
  }


  def mainly(args: Array[String]) {
    import dispatch._
    import dispatch.liftjson.Js._
    import net.liftweb.json.JsonAST._

    def http = new Http
    def search = :/("github.com") / "api" / "v2"/ "json" / "repos" / "search"
    val RepoNamed = """(\S+)\.g8""".r
    @annotation.tailrec
    def fetch(tmpls: List[Template], page: Int): List[Template] = {
      (for {
        repos <- http(search / "g8" <<? Map("start_page" -> page.toString) ># ('repositories ? ary))
        JObject(fields) <- repos
        JField("name", JString(repo)) <- fields
        JField("username", JString(user_name)) <- fields
        JField("description", JString(desc)) <- fields
        repo_name <- RepoNamed.findFirstMatchIn(repo)
      } yield Template(user_name, repo_name.group(1), desc)) match {
        case Nil =>
          println("exhausted g8 search on page %s" format page)
          tmpls
        case more => fetch(more ::: tmpls, page + 1)
      }
    }

    try {
      val tmpls = fetch(List.empty[Template], 1)
      for(t <- tmpls) println(t)
      println("resolved %d g8 templates" format tmpls.size)
      G8.save(tmpls)
    } catch {
      case StatusCode(404, _) =>
        println("Unable to find github repositories like g8")
    }
  }
}

