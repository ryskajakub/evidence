package bootstrap.liftweb

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import jr.model._
import net.liftweb.sitemap.Loc.{If, Value}
import jr.snippet.{LoggedAdmin, LoggedSeller}
import net.liftweb.util.{Props, Mailer}
import javax.mail.{Authenticator, PasswordAuthentication}
import provider.HTTPRequest
import jr.view.AutocompleteRest

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */

object Boot {

  def getIf(admin: Boolean = false, shopOwner: Boolean = false, seller: Boolean = false) = {
    If(() => {
      val res = (admin, shopOwner, seller) match {
        case (true, _, _) if LoggedAdmin.isAdmin => true
        case (_, true, _) if LoggedAdmin.isShopOwner => true
        case (_, _, true) if LoggedSeller.get.isDefined => true
        case _ => false
      }
      res
    }, () => RedirectResponse("/index"))
  }

  def getInversedIf = {
    If(() => {
      !(LoggedAdmin.isAdmin || LoggedAdmin.isShopOwner || LoggedSeller.get.isDefined)
    },() => RedirectResponse("/purchases"))
  }

  val newItem = Menu.param[Item]("item", "Položka", (x: String) => Full(DBStorer.getItem(x) openOr Item()),
    (item: Item) => item.id.toString) / "item" >> Value(Item()) >> getIf(true, true)
  val newBuyer = Menu.param[Buyer]("buyer", "Kupující", (x: String) => Full(DBStorer.getBuyer(x) openOr Buyer()),
    (buyer: Buyer) => buyer.id.toString) / "buyer" >> Value(Buyer()) >> getIf(false, false, true)
  val newSeller = Menu.param[Seller]("seller", "Prodavač", (x: String) => Full(DBStorer.getSeller(x) openOr Seller()),
    (seller: Seller) => seller.id.toString) / "seller" >> Value(Seller()) >> getIf(true)
  val shopOwner = Menu.param[ShopOwner]("shopOwner", "Majitel",
    (x: String) => Full(DBStorer.getShopOwner(x) openOr RealShopOwner()),
    (shopOwner: ShopOwner) => shopOwner.id.toString
  ) / "shopOwner" >> Value(RealShopOwner()) >> getIf(true)
  val cart = Menu("allitems", "Košík") / "cart" >> getIf(false, false, true)
  val purchases = Menu("purchases", "Prodeje") / "purchases" >> getIf(true, true, true)
  val withdraw = Menu("withdrawal", "Výběry") / "withdrawal" >> getIf(true, true, true)
  val balance = Menu("balance", "Bilance") / "balance" >> getIf(false, false, true)
  val transfers = Menu("transfers", "Placeno převodem") / "transfers" >> getIf(true)
  val index = Menu("index", "Login") / "index" >> getInversedIf
  val about = Menu("about", "O stránce") / "about" >> getInversedIf
  val evidence = Menu("evidence", "Seznam") / "evidence" >> getIf(true)
  val sell = Menu.param[String]("sell", "Prodej", (x: String) => {
    if (x == "transfer" || x == "cash")
      Full(x)
    else
      Empty
  }, x => x) / "sell" >> getIf(false, false, true) >> Loc.Hidden

}

class Boot {

  import Boot._

  def boot {
    Mailer.authenticator = (Props.get("mail.user"), Props.get("mail.password")) match {
      case (Full(u), Full(p)) => Full(new Authenticator {
        override def getPasswordAuthentication = new javax.mail.PasswordAuthentication(u, p)
      })
      case _ => Empty
    }
    LiftRules.statelessDispatchTable.append(AutocompleteRest)
    LiftRules.early.append(makeUtf8)
    DBStorer.init()
    // where to search snippet
    LiftRules.addToPackages("jr")
    // Build SiteMap
    val entries = index :: newItem :: newBuyer :: newSeller :: cart :: shopOwner :: sell ::
      purchases :: withdraw :: balance :: transfers :: /*withdrawals ::*/ about :: evidence :: Nil
    LiftRules.setSiteMap(SiteMap(entries: _*))
  }

  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

}

