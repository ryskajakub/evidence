import jr.model.{Item, DBStorer}

val groupedItems = DBStorer.allItem.groupBy((item:Item) => item.category)
val keys = groupedItems.toList.map(_._1)
