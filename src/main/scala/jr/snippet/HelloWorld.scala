package jr.snippet

import _root_.net.liftweb.util.Helpers
import jr.model._
import Helpers._
import _root_.scala.xml.{Text, NodeSeq}
import net.liftweb.common.{Box, Empty, Full}
import bootstrap.liftweb.Boot
import net.liftweb.sitemap.Loc
import net.liftweb.http.{RequestVar, S, SessionVar, SHtml}
import net.liftweb.http.js.JsCmds.Replace
import net.liftweb.http.js.{JsCmd, JsCmds}

object CartVar extends SessionVar[List[Item]](List()) {
  def clear() {
    CartVar(List())
  }

  def remove(item: Item) = {
    CartVar(this.get.filter(_ != item))
  }

  def add(value: Item) = {
    CartVar(value :: this.get)
  }

  def count(item: Item): Int = get.count((x: Item) => x == item)

  def total = {
    this.get.foldLeft(0)((total, a) => total + a.price)
  }
}

object Cart {
  def applyCart(cartPattern: NodeSeq): NodeSeq = {
    Helpers.bind(
      "items",
      cartPattern,
      "total" -%> Text(CartVar.total.toString),
      "repeat" -%> {
        (text: NodeSeq) => {
          val items = CartVar.get.distinct
          items.flatMap {
            (item: Item) => {
              Helpers.bind(
                "item",
                text,
                "name" -%> Text(item.title),
                "count" -%> Text(CartVar.count(item).toString),
                "price" -%> Text((item.price * CartVar.count(item)).toString),
                "remove" -%> {
                  "a" #> SHtml.onEvents("onclick") {
                    (x) => CartVar.remove(item)
                    Replace("cartTable", applyCart(cartPattern))
                  }
                }
              )
            }
          }
        }
      }
    )
  }

  def render(xhtml: NodeSeq) = {
    var resultPattern = NodeSeq.Empty
    var cartPattern = NodeSeq.Empty
    var onEnterFun : () => JsCmd = () => JsCmds.Noop
    def makeNodeSeq(results: List[Item], div: String) = {
      val res = Helpers.bind(
        "result",
        resultPattern,
        "row" -%> {
          (text: NodeSeq) => {
            results.flatMap((res: Item) => {
              Helpers.bind(
                "row",
                text,
                "name" -%> {
                  if (res.count - CartVar.count(res) > 0) {
                    "a *" #> res.title &
                      "a" #> SHtml.onEvents("onclick") {
                        x => {
                          CartVar.add(res)
                          JsCmds.Run("decrement(%s)".format(res.id)) &
                            JsCmds.Replace("cartTable", applyCart(cartPattern))
                        }
                      }
                  } else {
                    "a *" #> res.title
                  }
                },
                "code" -%> Text(res.code),
                "price" -%> Text(res.price.toString),
                "stock" -%> {
                  "td *" #> (res.count - CartVar.count(res)) &
                    "td [id]" #> ("item" + res.id)
                }
              )
            })
          }
        }
      )
      ("table [id]" #> div)(res)
    }
    Helpers.bind(
      "input",
      xhtml,
      "resultPattern" -%> {
        (text: NodeSeq) => {
          resultPattern = text
          NodeSeq.Empty
        }
      },
      "code" -%> {
        "input [onkeyup]" #> SHtml.onEvent((x: String) => {
          onEnterFun = {
            () =>
              DBStorer.getItemByCode(x) match {
                case Full(item) =>
                  CartVar.add(item)
                  Replace("cartTable", applyCart(cartPattern))
                case _ =>
                  JsCmds.Noop
              }
          }
          JsCmds.Noop
        }) &
        "a [onclick]" #> SHtml.onEvent((x: String) => {
          onEnterFun()
        })
      }  ,
      "category" -%> {
        "input" #> SHtml.onEvents("onkeyup") {
          (input: String) =>
            val results = DBStorer.allItem.filter {
              (item: Item) => {
                item.category.toLowerCase.contains(input.toLowerCase)
              }
            }.take(10)
            val result = makeNodeSeq(results, "categoryContent")
            JsCmds.Replace("categoryContent", result)
        }
      },
      "name" -%> {
        "input" #> SHtml.onEvents("onkeyup") {
          (input: String) =>
            val results = DBStorer.allItem.filter {
              (item: Item) => {
                item.title.toLowerCase.contains(input.toLowerCase)
              }
            }.take(10)
            val result = makeNodeSeq(results, "nameContent")
            JsCmds.Replace("nameContent", result)
        }
      },
      "cart" -%> {
        (seq: NodeSeq) => {
          cartPattern = seq
          applyCart(cartPattern)
        }
      }
    )
  }

}

object LoggedSeller extends SessionVar[Box[Seller]](Empty)

object LoggedAdmin extends SessionVar[Box[ShopOwner]](Empty) {
  def isShopOwner = this.get match {
    case Full(a: RealShopOwner) => true
    case _ => false
  }

  def getName = {
    this.get match {
      case Full(shopOwner) =>
        shopOwner.name
      case _ =>
        "nobody logged"
    }
  }

  def isAdmin = this.get match {
    case Full(Admin) => true
    case _ => false
  }
}

object Balance {
  def render(xhtml: NodeSeq) = {
    val balance = DBStorer.getBalance(LoggedSeller.get)
    Helpers.bind(
      "input",
      xhtml,
      "input" -%> Text(balance.map(_.toString).openOr("???"))
    )
  }
}

object MyMailer {

  def sendMail(seller: Seller, buyer: Buyer) {
    import net.liftweb.util.Mailer
    import Mailer._
    val myRecips: List[String] = List(buyer.email)
    val plainContent: String =
      "Ahoj, bylo ti prodáno toto zboží:\n" +
        CartVar.get.distinct.map(_.title).mkString("\n") +
        "Uhraď prosím částku " + CartVar.total + " na komunitní účet: 1021034503 / 5500.\n" +
        "Děkuje ti Yellow Gakyil a " + seller.name + ".\n"
    val myRecipsTo = myRecips.map(To(_))

    Mailer.sendMail(From("ryskajakub@seznam.cz"), Subject("Dzogchen dharmashop"),
      (PlainMailBodyType(plainContent) :: myRecipsTo): _*)
  }
}

object Transfers {

  def render(xhtml: NodeSeq) = {
    val idsUnpaied: scala.collection.mutable.Set[Purchase] = scala.collection.mutable.Set.empty
    val idsPaied: scala.collection.mutable.Set[Purchase] = scala.collection.mutable.Set.empty
    def transfers(xhtml2: NodeSeq): NodeSeq = {
      val unpaiedTransfers = DBStorer.getUnpaiedTransfers.map((_, false))
      val paiedTransfers = DBStorer.getPaiedTransfers.map((_, true))
      (unpaiedTransfers ::: paiedTransfers).flatMap((x: Pair[Purchase, Boolean]) => {
        val (purchase, paied) = x
        Helpers.bind(
          "transfer",
          xhtml2,
          "seller" -%> Text(purchase.seller),
          "buyer" -%> Text(if (purchase.buyer == null) "" else purchase.buyer),
          "date" -%> Text(purchase.date.toString),
          "total" -%> Text(purchase.total.toString),
          "tick" -%> SHtml.checkbox(false, (b: Boolean) => if (b) {
            if (paied) {
              idsPaied += purchase
            } else {
              idsUnpaied += purchase
            }
          }),
          "state" -%> Text(if (paied) "Zaplaceno" else "Nezaplaceno")
        )
      })
    }
    Helpers.bind(
      "one",
      xhtml,
      "transfers" -%> transfers _,
      "submit" -%> SHtml.submit("Provedeno", () => {
        DBStorer.tick(idsUnpaied.toSet, idsPaied.toSet)
        S.redirectTo("/transfers")
      })
    )
  }
}

object Withdrawals {
  def render(xhtml: NodeSeq) = {
    val admin = LoggedSeller.get.isEmpty
    Helpers.bind(
      "w",
      xhtml,
      "seller" -%> {
        (text: NodeSeq) => if (admin) text else NodeSeq.Empty
      },
      "repeat" -%> {
        (text: NodeSeq) => {
          val withdrawals = DBStorer.allWithdawals
          val filtered = if (admin) withdrawals
          else {
            withdrawals.filter(w => {
              val seller = LoggedSeller.get
              (w.seller, seller) match {
                case (Full(one), Full(two)) if (one.id == two.id) => true
                case _ => false
              }
            })
          }
          filtered.flatMap {
            (w) => {
              Helpers.bind(
                "one",
                text,
                "date" -%> Text(w.date.toString),
                "amount" -%> Text(w.amount.toString),
                "seller" -%> ((xml: NodeSeq) => {
                  if (admin) {
                    Helpers.bind(
                      "seller",
                      xml,
                      "bind" -%> Text(w.seller.map(_.name).getOrElse("n/a"))
                    )
                  } else {
                    NodeSeq.Empty
                  }
                })
              )
            }
          }
        }
      }
    )
  }
}

object Withdraw {
  def render(xhtml: NodeSeq) = {
    var seller: Seller = null
    var count: Int = 0
    val sellers = DBStorer.allSeller.map {
      (x: Seller) => (x, x.name)
    }
    Helpers.bind(
      "withdraw",
      xhtml,
      "input" -%> {
        (seq: NodeSeq) => {
          if (LoggedSeller.get.isDefined) {
            Helpers.bind(
              "input",
              seq,
              "seller" -%> SHtml.selectObj(sellers, Empty, (x: Seller) => seller = x),
              "amount" -%> SHtml.text("", (x: String) => count = x.toInt),
              "submit" -%> SHtml.submit("OK", () => DBStorer.withdraw(seller, count))
            )
          } else {
            NodeSeq.Empty
          }
        }
      },
      "list" -%> {
        (xhtml2: NodeSeq) =>
          val allWithdrawals = DBStorer.allWithdawals
          allWithdrawals.flatMap((withd: Withdraw) => {
            Helpers.bind(
              "list",
              xhtml2,
              "seller" -%> Text(withd.seller.map(x => x.name).openOr("unknown")),
              "amount" -%> Text(withd.amount.toString),
              "date" -%> Text(withd.date.toString)
            )
          })
      }
    )
  }
}

object LoggedIn {
  def render(xhtml: NodeSeq) = {
    Helpers.bind(
      "name",
      xhtml,
      "name" -%> Text(LoggedSeller.map(_.name) openOr (LoggedAdmin.getName)),
      "logout" -%> {
        (xhtml: NodeSeq) => {
          if (LoggedSeller.get.isDefined || LoggedAdmin.get.isDefined) {
            SHtml.link("/index", () => {
              LoggedAdmin(Empty)
              LoggedSeller(Empty)
            }, xhtml)
          } else {
            NodeSeq.Empty
          }
        }
      }
    )
  }
}

object Purchases {
  def render = {
    val purchases = DBStorer.getPurchases()
    "tbody *" #> purchases.map {
      (x: Purchase) =>
        val (id, count, price) = x.idCountPrice.head
        val rowspan = x.idCountPrice.size
        ".date *" #> x.date.toString &
          ".seller *" #> x.seller &
          ".buyer *" #> x.buyer &
          ".item *" #> DBStorer.getItem(id.toString).map(_.title).getOrElse("") &
          ".count *" #> count &
          ".price *" #> price &
          ".total *" #> x.total &
          ".date [rowspan]" #> rowspan &
          ".seller [rowspan]" #> rowspan &
          ".buyer [rowspan]" #> rowspan &
          ".total [rowspan]" #> rowspan &
          ".other *" #> x.idCountPrice.tail.map(x => {
            val (id2, count2, price2) = x
            ".item2 *" #> DBStorer.getItem(id2.toString).map(_.title).getOrElse("") &
              ".price2 *" #> price2 &
              ".count2 *" #> count2
          })
    }
  }
}

class Login {
  var seller = Seller()

  def render(xhtml: NodeSeq) = {
    Helpers.bind(
      "input",
      xhtml,
      "name" -%> SHtml.text("", (x: String) => seller = seller.copy(name = x)),
      "password" -%> SHtml.password("", (x: String) => seller = seller.copy(password = x)),
      "submit" -%> SHtml.submit("Přihlásit", () => {
        DBStorer.login(seller.name, seller.password) match {
          case Full(Left(seller1)) => LoggedSeller(Full(seller1))
          case Full(Right(owner)) => LoggedAdmin(Full(owner))
          case _ =>
        }
      })
    )
  }
}

class Code {
  def render(xhtml: NodeSeq) = {
    Helpers.bind(
      "code",
      xhtml,
      "code" -%> SHtml.ajaxText("", true, (x: String) => {
        val item = DBStorer.getItemByCode(x.trim)
        item match {
          case Full(item) =>
            val itCount = item.count - CartVar.count(item)
            if (itCount > 0) {
              CartVar(item :: CartVar.get)
            } else {
              Message("není na skladě!")
            }
          case _ =>
        }
        JsCmds.RedirectTo(Boot.cart.toMenu.loc.calcDefaultHref)
      })
    )
  }
}

object Mail {

}


object Message extends RequestVar[String]("")

class EditItem(in: Item) {
  var item = in
  var count: Int = 0
  var note: String = ""

  def render(xhtml: NodeSeq) = {
    Helpers.bind(
      "input",
      xhtml,
      "addings" -%> {
        (seq: NodeSeq) => {
          in.getAddings.flatMap {
            (adding: ItemAdd) => {
              Helpers.bind(
                "adding",
                seq,
                "note" -%> Text(adding.note),
                "date" -%> Text(adding.date.toString),
                "count" -%> Text(adding.count.toString)
              )
            }
          }
        }
      },
      "message" -%> {
        (par: NodeSeq) => Helpers.bind("message", par, "content" -%> Text(Message.get))
      },
      "title" -%> SHtml.text(in.title, (x) => item = item.copy(title = x)),
      "category" -%> SHtml.text(in.category, (x) => item = item.copy(category = x)),
      "code" -%> SHtml.text(in.code, (x) => item = item.copy(code = x)),
      "price" -%> SHtml.text(in.price.toString, (x) => item = item.copy(price = x.trim.toInt)),
      "submit" -%> SHtml.submit("OK", () => DBStorer.saveItem(item)),
      "add" -%> {
        (xhtml2: NodeSeq) =>
          item.id match {
            case x if x > 0 =>
              Helpers.bind(
                "item",
                xhtml2,
                "count" -%> SHtml.text(in.count.toString, (x) => count = x.toInt),
                "note" -%> SHtml.text("", (x) => note = x),
                "add" -%> SHtml.submit("OK", () => {
                  DBStorer.addToItem(item, count, note)
                  Message("zboží bylo naskladněno")
                }),
                "message" -%> Text(Message.get)
              )
            case _ => NodeSeq.Empty
          }
      })
  }
}

class EditBuyer(in: Buyer) {

  var buyer = in

  def render(xhtml: NodeSeq) = {
    Helpers.bind(
      "input",
      xhtml,
      "name" -%> SHtml.text(in.name, (x) => buyer = buyer.copy(name = x)),
      "email" -%> SHtml.text(in.email, (x) => buyer = buyer.copy(email = x)),
      "submit" -%> SHtml.submit("OK", () => DBStorer.saveBuyer(buyer))
    )
  }
}

class EditShopOwner(in: RealShopOwner) {

  var shopOwner = in

  def render(xhtml: NodeSeq) = {
    Helpers.bind(
      "input",
      xhtml,
      "name" -%> SHtml.text(in.name, (x) => shopOwner = shopOwner.copy(name = x)),
      "password" -%> SHtml.text(in.password, (x) => shopOwner = shopOwner.copy(password = x)),
      "submit" -%> SHtml.submit("OK", () => DBStorer.saveOwner(shopOwner))
    )
  }
}

class Sell(x: String) {
  val cash = (x == "cash")

  def render(xhtml: NodeSeq) = {
    var message: String = ""
    var buyer: Buyer = Buyer()
    var emailTemplate = NodeSeq.Empty
    var newPerson = Buyer()
    Helpers.bind(
      "input",
      xhtml,
      "total" -%> Text(CartVar.total.toString),
      "searchedEmail" -%> (x => {
        emailTemplate = x
        x
      }),
      "searchPerson" -%>
        "input [onkeyup]" #> SHtml.onEvent((x: String) => {
          val person = DBStorer.getBuyerByFullName(x)
          val res = person match {
            case Full(per) =>
              buyer = per
              ("span *" #> per.email)(emailTemplate)
            case _ =>
              buyer = Buyer()
              ("span *" #> "")(emailTemplate)
          }
          JsCmds.Replace("email", res)
        }),
      "newPerson" -%> SHtml.text("", (x) => {
        newPerson.copy(name = x)
      }),
      "newEmail" -%> SHtml.text("", (x) => {
        newPerson.copy(email = x)
      }),
      "message" -%> Text(message),
      "counter" -%> ((x) => if (cash) x else NodeSeq.Empty),
      "ok" -%> SHtml.submit("Prodej", () => {
        val choosenBuyer = if (newPerson.isEmpty) buyer else newPerson
        (LoggedSeller.get, choosenBuyer) match {
          case (Full(seller), buyer1) if seller.isValid && buyer1.isValid =>
            DBStorer.sell(CartVar.get, seller, buyer1, if (cash) 0 else 1)
            if (!cash) {
              MyMailer.sendMail(seller, buyer)
            }
            CartVar.clear()
          case _ =>
        }
      })
    )
  }
}

object AllHelper {
  def all[A](all: List[A], name: A => String, loc: Loc[A], xhtml: NodeSeq, code: A => NodeSeq = ((x: A) => NodeSeq.Empty)) = {
    Helpers.bind(
      "objects",
      xhtml,
      "list" -%> {
        (xhtml2: NodeSeq) =>
          all.flatMap((obj: A) => {
            Helpers.bind(
              "object",
              xhtml2,
              "name" -%> Text(name(obj)),
              "code" -%> code(obj),
              "edit" -%> {
                (html: NodeSeq) =>
                  <a href={loc.calcHref(obj)}>
                    {html}
                  </a>
              }
            )
          })
      }
    )
  }
}

object AllShopOwners {
  def render(xhtml: NodeSeq) = {
    AllHelper.all(DBStorer.allShopOwners, (x: ShopOwner) => x.name, Boot.shopOwner.toLoc, xhtml)
  }
}

object AllItems {
  def render(xhtml: NodeSeq) = {
    AllHelper.all(DBStorer.allItem.filter((item: Item) => {
      val ownerId = item.owner
      LoggedAdmin.get match {
        case Full(shopOwner) if shopOwner.id == ownerId => true
        case _ => false
      }
    }),
      (x: Item) => x.title, Boot.newItem.toLoc, xhtml, (x: Item) => Text(x.code))
  }
}

object AllSellers {
  def render(xhtml: NodeSeq) = {
    AllHelper.all(DBStorer.allSeller, (x: Seller) => x.name, Boot.newSeller.toLoc, xhtml)
  }
}

object AllBuyers {

  def render(xhtml: NodeSeq) = {
    AllHelper.all(DBStorer.allBuyer, (x: Buyer) => x.name, Boot.newBuyer.toLoc, xhtml)
  }
}

class EditSeller(in: Seller) {
  var seller = in

  def render(xhtml: NodeSeq) = {
    Helpers.bind(
      "input",
      xhtml,
      "name" -%> SHtml.text(in.name, (x) => seller = seller.copy(name = x)),
      "password" -%> SHtml.text(in.password, (x) => seller = seller.copy(password = x)),
      "submit" -%> SHtml.submit("OK", () => DBStorer.saveSeller(seller))
    )
  }
}

object Evidence {
  def render(xhtml: NodeSeq) = {
    val groupedItems: Map[String, List[Item]] = DBStorer.allItem.groupBy((item: Item) => item.category)
    val eithers: List[Either[String, Item]] =
      groupedItems.toList.map {
        case (category, items) => Left(category) :: items.map(Right(_))
      }.flatten
    val size = 3
    def take[A](fromList: List[A], toList: List[List[A]]): List[List[A]] = {
      if (fromList.size <= size) {
        toList :+ fromList
      } else {
        val (taken, rest) = fromList.splitAt(size)
        take(rest, toList :+ taken)
      }
    }
    val splitted = take(eithers, Nil)
    splitted.flatMap {
      (table) =>
        Helpers.bind(
          "tables",
          xhtml,
          "repeat" -%> {
            (templ1: NodeSeq) => {
              Helpers.bind(
                "categories",
                templ1,
                "repeat" -%> {
                  (templ2: NodeSeq) => {
                    def fromEitherToNodeSeq(either: Either[String, Item]) = {
                      either match {
                        case Left(category) =>
                          Helpers.bind("category", templ2,
                            "name" -%> {
                              (templ3: NodeSeq) => Helpers.bind("name", templ3, "name" -%> Text(category))
                            },
                            "items" -%> NodeSeq.Empty
                          )
                        case Right(item) =>
                          val bind: NodeSeq = Helpers.bind("category", templ2,
                            "name" -%> NodeSeq.Empty,
                            "items" -%> {
                              (templ4: NodeSeq) =>
                                Helpers.bind(
                                  "item",
                                  templ4,
                                  "name" -%> Text(item.title),
                                  "code" -%> Text(item.code),
                                  "price" -%> Text(item.price.toString),
                                  "count" -%> Text(item.count.toString)
                                )
                            }
                          )
                          bind
                      }
                    }
                    table.flatMap((row) => fromEitherToNodeSeq(row))
                  }
                }
              )
            }
          }
        )
    }
  }
}

