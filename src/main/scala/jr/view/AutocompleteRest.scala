package jr.view

import net.liftweb.http.rest.RestHelper
import net.liftweb.json.Serialization
import net.liftweb.http.{PlainTextResponse, GetRequest, Req}
import jr.model.DBStorer

/**
 * User: jry
 * Date: 5/29/12
 * Time: 2:09 PM
 */

object AutocompleteRest extends RestHelper {
  serve {
    case Req("autocomplete" :: "buyer" :: name :: Nil, _, GetRequest) =>
      val namesFound = DBStorer.getBuyerByLike(name)
      val serial = Serialization.write(namesFound)
      PlainTextResponse(serial)
    case Req("autocomplete" :: "category" :: name :: Nil, _, GetRequest) =>
      val namesFound = DBStorer.getCategoryByLike(name)
      val serial = Serialization.write(namesFound)
      PlainTextResponse(serial)
  }

}
