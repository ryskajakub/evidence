package jr.model

import org.apache.commons.dbutils.ResultSetHandler
import org.apache.commons.dbcp.{PoolingDataSource, PoolableConnectionFactory, DriverManagerConnectionFactory}
import org.apache.commons.pool.impl.GenericObjectPool
import org.apache.commons.dbutils.QueryRunner
import java.sql.ResultSet
import net.liftweb.common.Full
import net.liftweb.common.Empty
import net.liftweb.common.Box
import java.util.{Date, Properties}

case class Item(id: Int = 0, title: String = "", category: String = "", owner: Int = 0, price: Int = 0,
                code: String = "") {
  def count = DBStorer.countAdded(id) - DBStorer.countSold(id)

  def getAddings = {
    DBStorer.getAddings(id)
  }
}

case class Seller(id: Int = 0, name: String = "", password: String = "", balance: Int = 0) {
  def isValid = id != 0
}

case class Buyer(id: Int = 0, name: String = "", email: String = "") {
  def isValid = id != 0


  def isEmpty: Boolean = {
    name.trim == "" && email.trim == ""
  }
}

case class ItemAdd(count: Int, note: String, date: Date)

trait ShopOwner {
  def id: Int

  def name: String

  def password: String

  def isAdmin: Boolean
}

case class RealShopOwner(id: Int = 0, name: String = "", password: String = "") extends ShopOwner {
  def isAdmin = false
}

case object Admin extends ShopOwner {
  def id = 0

  def name = "admin"

  def password = "admin"

  def isAdmin = true
}

case class SellerHistory(id: Int = 0, sellerId: Int = 0, text: String = "")

case class Withdraw(sellerId: Int = 0, amount: Int = 0, date: Date = new Date) {
  def seller = DBStorer.getSeller(sellerId.toString)
}

case class Purchase(buyer: String, seller: String, date: Date, id: Int, type1: Int) {

  def idCountPrice = DBStorer.idCountPrice(id)

  def total = idCountPrice.foldLeft(0)((total, triple) => total + triple._2 * triple._3)

  def items = idCountPrice.map {
    x => DBStorer.getItem(x._1.toString)
  }

  def itemsCountPrice = idCountPrice.map {
    x => (DBStorer.getItem(x._1.toString), x._2, x._3)
  }
}

/**
 * Created by IntelliJ IDEA.
 * User: coub
 * Date: 2/11/12
 */

object DBStorer {
  def getAddings(i: Int) = {
    queryRunner.query("select count, note, date1 from ev_add_item where item_id = ?",
      funToResultSetHandlerMany(queryAdding), i.asInstanceOf[AnyRef])
  }

  def getCategoryByLike(s: String) = {
    queryRunner.query("select distinct category from ev_item where category like ?",
      funToResultSetHandlerMany(queryString), "%" + s + "%")
  }

  def getBuyerByFullName(name: String): Box[Buyer] = {
    queryRunner.query("select buyer_id, name, email from ev_buyer where name = ?",
      funToResultSetHandler(queryBuyer), name)
  }

  def getBuyerByLike(value: String) = {
    queryRunner.query("select name from ev_buyer where name like ?",
      funToResultSetHandlerMany(queryString), "%" + value + "%")
  }

  def tick(unpaied: Set[Purchase], paied: Set[Purchase]) {
    unpaied.foreach((x: Purchase) => {
      queryRunner.update(
        """
         update ev_purchase set type = 2 where purchase_id = ?
        """, x.id)
    })
    paied.foreach((x: Purchase) => {
      queryRunner.update(
        """
         update ev_purchase set type = 1 where purchase_id = ?
        """, x.id)
    })
  }

  def getUnpaiedTransfers = {
    getPurchases().filter(_.type1 == 1)
  }

  def getPaiedTransfers = {
    getPurchases().filter(_.type1 == 2)
  }

  def getBalance(box: Box[Seller]): Box[Int] = {
    box.map((x: Seller) => {
      val sells = getPurchases(Full(x.id)).filter(_.type1 == 0)
      val amountGot = sells.map {
        p => {
          val id = p.id
          val total: List[Int] = idCountPrice(id).map {
            case (id, count, price) => count * price
          }
          total.foldLeft(0)(_ + _)
        }
      }.foldLeft(0)(_ + _)
      val amountGave: Box[Int] = queryRunner.query(
        """
          select sum(amount) from ev_withdraw_money where seller_id = ?
        """, funToResultSetHandler(x => x.getInt(1)), x.id.asInstanceOf[AnyRef])
      for {
        gave <- amountGave
      } yield (amountGot - gave)
    }).flatMap(x => x)
  }

  def allWithdawals = {
    queryRunner.query( """
        select seller_id, date, amount from ev_withdraw_money
                       """, funToResultSetHandlerMany(x =>
      Withdraw(sellerId = x.getInt(1), date = x.getDate(2), amount = x.getInt(3))
    ))
  }

  def withdraw(seller: Seller, amount: Int) = {
    queryRunner.update("insert into ev_withdraw_money(seller_id, date, amount) values(?, NOW(), ?)",
      seller.id.asInstanceOf[AnyRef], amount.asInstanceOf[AnyRef]
    )
  }

  def idCountPrice(purchaseId: Int) =
    queryRunner.query( """
        select count, item_price, item_id from(
          select item_price_id, max(valid_from) from ev_purchase
          join ev_purchase_item using (purchase_id)
          join ev_item_price using(item_id)
          where purchase_id = ?
          and valid_from <= date1
          group by item_id
        ) as it
        join ev_item_price using(item_price_id)
        join ev_purchase_item using(item_id)
        where purchase_id = ?
                       """, funToResultSetHandlerMany(x => (x.getInt(3), x.getInt(1), x.getInt(2))),
      purchaseId.asInstanceOf[AnyRef], purchaseId.asInstanceOf[AnyRef])


  def getPurchases(sellerId: Box[Int] = Empty) = {
    val where = sellerId.map(int => "where ev_seller.seller_id = " + int.toString).openOr("")
    val purchases = queryRunner.query( """
        select purchase_id, ev_buyer.name, ev_seller.name, date1, type
        from ev_purchase
        join ev_seller using(seller_id)
        left join ev_buyer on(buyer = ev_buyer.buyer_id)
                                       """ + where,
      funToResultSetHandlerMany(x => Purchase(x.getString(2), x.getString(3),
        x.getDate(4), x.getInt(1), x.getInt(5))))
    purchases
  }

  def countAdded(i: Int): Int = {
    queryRunner.query("select sum(count) from ev_add_item where item_id = ?", funToResultSetHandler(x =>
      x.getInt(1)), i.asInstanceOf[AnyRef]) match {
      case Full(x) => x
      case _ => 0
    }
  }

  def countSold(i: Int): Int = {
    queryRunner.query("select sum(count) from ev_purchase_item where item_id = ?", funToResultSetHandler(x =>
      x.getInt(1)), i.asInstanceOf[AnyRef]) match {
      case Full(x) => x
      case _ => 0
    }
  }

  Class.forName("com.mysql.jdbc.Driver")
  val props = new Properties
  props.setProperty("user", "crm")
  props.setProperty("password", "crm")
  props.setProperty("characterEncoding", "UTF-8")
  props.setProperty("characterSetResults", "UTF-8")
  val url = "jdbc:mysql://localhost:3306/crm"
  val connectionFactory = new DriverManagerConnectionFactory(url, props)
  val connectionPool = new GenericObjectPool(null);
  val poolableFactory = new PoolableConnectionFactory(connectionFactory, connectionPool, null, null, false, true)
  val poolingDataSource = new PoolingDataSource(connectionPool)
  val queryRunner = new QueryRunner(poolingDataSource)

  val queryItem = (x: ResultSet) => {
    val itemId = x.getInt(1)
    val title = x.getString(2)
    val category = x.getString(3)
    val owner = x.getInt(4)
    val code = x.getString(5)
    Item(itemId, title, category, owner, _: Int, code)
  }

  def queryString: (ResultSet) => String = {
    (x: ResultSet) => x.getString(1)
  }

  def queryShopOwner: (ResultSet) => ShopOwner = {
    (x: ResultSet) => {
      val id = x.getInt(1)
      val name = x.getString(2)
      val password = x.getString(3)
      RealShopOwner(id, name, password)
    }
  }

  val queryAdding =
    (x: ResultSet) => {
      val count = x.getInt(1)
      val note = x.getString(2)
      val date = x.getDate(3)
      ItemAdd(count, note, date)
    }

  val querySeller =
    (x: ResultSet) => {
      val sellerId = x.getInt(1)
      val name = x.getString(2)
      val password = x.getString(3)
      val balance = x.getInt(4)
      Seller(sellerId, name, password, balance)
    }

  val queryBuyer =
    (x: ResultSet) => {
      val buyerId = x.getInt(1)
      val name = x.getString(2)
      val email = x.getString(3)
      Buyer(buyerId, name, email)
    }

  val querySellerHistory =
    (x: ResultSet) => {
      val sellerHistoryId = x.getInt(1)
      val sellerId = x.getInt(2)
      val text = x.getString(3)
      SellerHistory(sellerHistoryId, sellerId, text)
    }

  def saveItemPrice(itemId: Int, price: Int) {
    queryRunner.update(
      "insert into ev_item_price(item_price, valid_from, item_id) values(?,NOW(),?)",
      price.asInstanceOf[AnyRef], itemId.asInstanceOf[AnyRef]
    )
  }

  def saveItem(item: Item) {
    if (getItem(item.id.toString).isDefined) {
      queryRunner.update(
        "update ev_item set title = ?, category = ?, shop_owner_id = ?, code = ? where item_id = ?",
        item.title, item.category, item.owner.asInstanceOf[AnyRef], item.code, item.id.asInstanceOf[AnyRef])
      if (mostRecentPrice(item.id.toString).get != item.price) {
        saveItemPrice(item.id, item.price)
      }
    } else {
      queryRunner.update(
        "insert into ev_item(title, category, shop_owner_id, code) values (?,?,?,?)",
        item.title, item.category, item.owner.asInstanceOf[AnyRef], item.code
      )
      val itemId = queryRunner.query("select LAST_INSERT_ID()", funToResultSetHandler((rs: ResultSet) => rs.getInt(1)))
      saveItemPrice(itemId.get, item.price)
    }
  }

  def saveSeller(seller: Seller) {
    if (getOwnerByName(seller.name).isEmpty && seller.name != "admin") {
      if (getSeller(seller.id.toString).isDefined) {
        queryRunner.update(
          "update ev_seller set name = ?, password = ?, balance = ? where seller_id = ?",
          seller.name, seller.password, seller.balance.asInstanceOf[AnyRef], seller.id.asInstanceOf[AnyRef]
        )
      } else {
        queryRunner.update(
          "insert into ev_seller(name, password, balance) values (?,?,?)",
          seller.name, seller.password, seller.balance.asInstanceOf[AnyRef]
        )
      }
    }
  }

  def saveOwner(owner: RealShopOwner) {
    if (getSellerByName(owner.name).isEmpty && owner.name != "admin") {
      if (getShopOwner(owner.id.toString).isDefined) {
        queryRunner.update(
          "update ev_shop_owner set name = ?, password = ?, balance = ? where seller_id = ?",
          owner.name, owner.password, owner.id.asInstanceOf[AnyRef]
        )
      } else {
        queryRunner.update(
          "insert into ev_shop_owner(name, password) values (?,?)",
          owner.name, owner.password
        )
      }
    }
  }

  def saveBuyer(buyer: Buyer): Buyer = {
    if (getBuyer(buyer.id.toString).isDefined) {
      queryRunner.update(
        "update ev_buyer set name = ?, email = ? where buyer_id = ?",
        buyer.name, buyer.email, buyer.id.asInstanceOf[AnyRef])
      buyer
    } else {
      queryRunner.update(
        "insert into ev_buyer(name,email) values (?,?)",
        buyer.name, buyer.email
      )
      val buyerId = queryRunner.query("select LAST_INSERT_ID()", funToResultSetHandler((rs: ResultSet) => rs.getInt(1)))
      getBuyer(buyerId.get.toString).get
    }
  }

  def insertSellerHistory(sellerHistory: SellerHistory) {
    queryRunner.update(
      "insert into seller_history(seller_id, text) values (?,?)",
      sellerHistory.sellerId.asInstanceOf[AnyRef], sellerHistory.text
    )
  }

  def mostRecentPrice(x: String) = priceAtDay(x, new Date())

  def priceAtDay(x: String, d: Date) = {
    queryRunner.query("select item_price, max(valid_from) from ev_item_price where item_id = ? and valid_from <= ? group by item_id",
      funToResultSetHandler(x => x.getInt(1)), x, d)
  }

  def getItemByCode(x: String) = {
    val result: Box[Int => Item] = queryRunner.query("select item_id, title, category, shop_owner_id, code from ev_item where code = ?",
      funToResultSetHandler(queryItem), x)
    val mostRecPr = (result.flatMap(x => mostRecentPrice(x.apply(0).id.toString)))
    (result, mostRecPr) match {
      case (Full(q), Full(mrp)) =>
        Full(q(mrp))
      case _ =>
        Empty
    }
  }

  def getItem(x: String): Box[Item] = {
    val result = queryRunner.query("select item_id, title, category, shop_owner_id, code from ev_item where item_id = " + x,
      (funToResultSetHandler(queryItem)))
    (result, mostRecentPrice(x)) match {
      case (Full(q), Full(mrp)) =>
        Full(q(mrp))
      case _ =>
        Empty
    }

  }

  def allItem = {
    queryRunner.query("select item_id, title, category, shop_owner_id, code from ev_item order by category,title desc",
      funToResultSetHandlerMany(queryItem)).map(f => {
      val price = mostRecentPrice(f(0).id.toString).get
      f(price)
    })
  }

  def getSeller(x: String) = {
    queryRunner.query("select seller_id, name, password, balance from ev_seller where seller_id = " + x,
      funToResultSetHandler(querySeller))
  }


  def getOwnerByName(s: String): Box[ShopOwner] = {
    queryRunner.query("select shop_owner_id, name, password from ev_shop_owner where name = ?",
      funToResultSetHandler(queryShopOwner), s)
  }

  def getSellerByName(x: String) = {
    queryRunner.query("select seller_id, name, password, balance from ev_seller where name = ?",
      funToResultSetHandler(querySeller), x)
  }

  def allSeller = {
    queryRunner.query("select seller_id, name, password, balance from ev_seller order by name desc",
      funToResultSetHandlerMany(querySeller))
  }

  def getBuyer(x: String) = {
    queryRunner.query("select buyer_id, name, email from ev_buyer where buyer_id = " + x,
      funToResultSetHandler(queryBuyer))
  }

  def allShopOwners = {
    queryRunner.query("select shop_owner_id, name, password from ev_shop_owner",
      funToResultSetHandlerMany(queryShopOwner))
  }

  def getShopOwner(s: String): Box[ShopOwner] = {
    queryRunner.query("select shop_owner_id, name, password from ev_shop_owner where shop_owner_id = " + s,
      funToResultSetHandler(queryShopOwner))
  }

  def allBuyer: List[Buyer] = {
    queryRunner.query("select buyer_id, name, email from ev_buyer order by name desc",
      funToResultSetHandlerMany(queryBuyer))
  }

  def getSellerHistory(x: String) = {
    queryRunner.query("select seller_history_id, seller_id, text from ev_seller_history where seller_history_id = " + x,
      funToResultSetHandler(querySellerHistory))
  }

  def allSellerHistory = {
    queryRunner.query("select seller_history_id, seller_id, text from ev_seller_history",
      funToResultSetHandlerMany(querySellerHistory))
  }

  //case class Item(id: Int, count: Int, title: String, category: String, price: Int, owner: String)
  val Tables = List( """
    ev_item (
      item_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      title VARCHAR(200) UNIQUE,
      category VARCHAR(1000),
      shop_owner_id INTEGER UNSIGNED,
      code VARCHAR(50) UNIQUE
    )
                     """,
    """
    ev_withdraw_money(
      id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      seller_id INTEGER UNSIGNED,
      date DATE,
      amount INTEGER
    )
    """,
    """
      ev_item_price (
        item_price_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        item_price INTEGER UNSIGNED,
        valid_from DATE,
        item_id INTEGER UNSIGNED
      )
    """,
    //case class Seller(id: Int, name: String, password: String, balance: Int)
    """
      ev_seller (
        seller_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(200) UNIQUE,
        password VARCHAR(1000),
        balance INTEGER
      )
    """,
    """
      ev_buyer (
        buyer_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        email VARCHAR(1000),
        name VARCHAR(200) UNIQUE
      )
    """,
    //case class SellerHistory(id: Int, sellerId: Int, text: String)
    """
      ev_seller_history (
        seller_history_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        seller_id INTEGER UNSIGNED,
        text VARCHAR(1000)
      )
    """
    ,
    // type  0 = cash 1 = transfer not complete 2 = transfer complete
    """
      ev_purchase (
        purchase_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        buyer INTEGER UNSIGNED,
        date1 DATE,
        type INTEGER UNSIGNED,
        seller_id INTEGER UNSIGNED
      )
    """
    ,
    """
      ev_purchase_item (
        purchase_id INTEGER UNSIGNED,
        count INTEGER UNSIGNED,
        item_id INTEGER UNSIGNED
      )
    """
    ,
    """
      ev_add_item(
        add_item_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        count INTEGER,
        note VARCHAR(1000),
        item_id INTEGER UNSIGNED,
        date1 DATE
      )
    """
    ,
    """
      ev_shop_owner(
        shop_owner_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(200) UNIQUE,
        password VARCHAR(1000)
      )
    """
  )

  def init() = {
    val before = "create table if not exists "
    val after = "engine = InnoDB"
    Tables.foreach {
      t: String =>
        try {
          queryRunner.update(before + t + after)
        } catch {
          case tr: Throwable =>
            println(t)
            println(tr)
        }
    }
  }

  def funToResultSetHandler[A](x: ResultSet => A): ResultSetHandler[Box[A]] = {
    new ResultSetHandler[Box[A]] {
      def handle(p1: ResultSet): Box[A] = {
        if (p1.next()) {
          Full[A](x.apply(p1))
        } else {
          Empty
        }
      }
    }
  }

  def funToResultSetHandlerMany[A](x: ResultSet => A): ResultSetHandler[List[A]] = {
    new ResultSetHandler[List[A]] {
      def handle(p1: ResultSet) = {
        def addList(list: List[A]): List[A] = {
          if (p1.next()) {
            addList(x.apply(p1) :: list)
          } else {
            list
          }
        }
        addList(Nil)
      }
    }
  }

  def sell(list: List[Item], seller: Seller, buyer: Buyer, type1: Int) {
    queryRunner.update("insert into ev_purchase(buyer, seller_id, date1, type) values (?,?, NOW(),?) ",
      if (buyer.id == 0) null else buyer.id.asInstanceOf[AnyRef],
      seller.id.asInstanceOf[AnyRef], type1.asInstanceOf[AnyRef]
    )
    val purchaseId = queryRunner.query("select LAST_INSERT_ID()", funToResultSetHandler((rs: ResultSet) => rs.getInt(1)))
    for (item <- list.distinct) {
      val count: Int = list.count((x: Item) => x == item)
      queryRunner.update("insert into ev_purchase_item(count, purchase_id, item_id) values(?,?,?)",
        count.asInstanceOf[AnyRef], purchaseId.get.asInstanceOf[AnyRef], item.id.asInstanceOf[AnyRef])
    }
  }

  def login(name: String, password: String): Box[Either[Seller, ShopOwner]] = {
    val sellerByName = getSellerByName(name)
    val ownerByName = getOwnerByName(name)
    val res = (sellerByName, ownerByName) match {
      case (Full(seller), _) if seller.password == password => Full(Left(seller))
      case (_, Full(owner)) if owner.password == password => Full(Right(owner))
      case _ if name == "admin" && password == "admin" => Full(Right(Admin))
      case _ => Empty
    }
    res
  }

  def addToItem(item: Item, i: Int, note: String) {
    queryRunner.update("insert into ev_add_item(count, item_id, date1, note) values (?,?,NOW(),?)",
      i.asInstanceOf[AnyRef], item.id.asInstanceOf[AnyRef], note)
  }
}

