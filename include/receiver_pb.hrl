-ifndef(PRICECHANGEACKSIMPLEMESSAGE_PB_H).
-define(PRICECHANGEACKSIMPLEMESSAGE_PB_H, true).
-record(pricechangeacksimplemessage, {
    refid,
    price,
    quantity,
    buyer,
    seller,
    direction,
    netchgprevday,
    variation,
    volume,
    condition
}).
-endif.

