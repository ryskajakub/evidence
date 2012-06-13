select count, item_price, item_id from(
	select item_price_id, max(valid_from) from ev_purchase
	join ev_purchase_item using (purchase_id)
	join ev_item_price using(item_id)
	where purchase_id = 3
	and valid_from <= date1
	group by item_id
) as it
join ev_item_price using(item_price_id)
join ev_purchase_item using(item_id)
where purchase_id = 3
