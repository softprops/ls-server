package ls

object Mongo {
  import com.mongodb.casbah.commons.Imports.ObjectId
  import com.mongodb.casbah.commons.{ MongoDBObject => Obj, MongoDBList => ObjList }
  import com.mongodb.casbah.{MongoCollection, MongoCursor}
  import com.mongodb.{ BasicDBList, DBObject }
  import com.mongodb.casbah.Implicits._

  type CanConvertListTo[A] = Iterator[DBObject] => Iterable[A]
  type CanConvertTo[A, B] = A => B

  // todo: contribute back to casbah
  implicit def mdbo2optpp(dbo: DBObject) = new {
    def opt(opt: Option[DBObject]): DBObject = opt match {
      case Some(other) => dbo ++ other
      case _ => dbo
    }
  }
  
  def anycase(term: String) = """(?i)%s""".format(term).r

  def narrowAnycase(term: String) = """(?i)^%s$""".format(term).r

  def paginate(c: MongoCursor, page: Int, lim: Int) =
    c.skip(math.max(0, (lim * (page - 1)) - 1)).limit(lim + 1)
}
