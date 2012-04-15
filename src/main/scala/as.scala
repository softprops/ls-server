package ls

object As extends Logged {
  import unfiltered.response.{ JsonContent, ResponseString, NotFound }
  def jsonTemplate(tmpls: Iterable[G8.Template]) =
     if(tmpls.isEmpty) NotFound
    else JsonContent ~> {
      log.info("returning %d templates" format tmpls.size)
      ResponseString(com.codahale.jerkson.Json.generate(tmpls))
    }
  def json(libs: Iterable[LibraryVersions]) =
    if(libs.isEmpty) NotFound
    else JsonContent ~> {
      log.info("returning %d libs" format libs.size)
      ResponseString(com.codahale.jerkson.Json.generate(libs))
    }

  def jsonVersion(v: Iterable[String]) =
    v.iterator match {
      case it if(it.hasNext) =>
         JsonContent ~>
          ResponseString(
            """{"version":"%s"}""" format it.next
          )
      case _ => NotFound
    }
}
