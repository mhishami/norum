-module(word_util).
-author ('Hisham Ismail <mhishami@gmail.com').


-export ([gen_phrase_name/0]).

-define (WOODS, [
            <<"Alder">>, <<"Ash">>, <<"Balsa">>, <<"Basswood">>, 
            <<"Beech">>, <<"Birch">>, <<"Boxwood">>, <<"Brazilwood">>, 
            <<"Bubinga">>, <<"Butternut">>, <<"Cherry">>, <<"Chestnut">>,
            <<"Cocobolo">>, <<"Ebony">>, <<"Elm">>, <<"Hickory">>, 
            <<"Jelutong">>, <<"Kingwood">>, <<"Lime">>, <<"Mahogany">>,
            <<"Maple">>, <<"Oak">>, <<"Plane">>, <<"Purpleheart">>,
            <<"Rosewood">>, <<"Sycamore">>, <<"Teak">>, <<"Tulipwood">>
    ]).
-define (METALS, [
            <<"Lithium">>, <<"Beryllium">>, <<"Sodium">>, <<"Magnesium">>,
            <<"Aluminum">>, <<"Potassium">>, <<"Calcium">>, <<"Scandium">>, 
            <<"Titanium">>, <<"Vanadium">>, <<"Chromium">>, <<"Manganese">>, 
            <<"Iron">>, <<"Cobalt">>, <<"Nickel">>, <<"Copper">>, 
            <<"Zinc">>, <<"Gallium">>, <<"Rubidium">>, <<"Strontium">>, 
            <<"Yttrium">>, <<"Zirconium">>, <<"Niobium">>, <<"Molybdenum">>, 
            <<"Technetium">>, <<"Ruthenium">>, <<"Rhodium">>, <<"Palladium">>, 
            <<"Silver">>, <<"Cadmium">>, <<"Indium">>, <<"Tin">>, 
            <<"Cesium">>, <<"Barium">>, <<"Lanthanum">>, <<"Cerium">>
    ]).

-define (LAKES, [
            <<"Gowd">>, <<"Kaptai">>, <<"Fewa">>, <<"Rara">>, 
            <<"Rupa">>, <<"Begnas">>, <<"Shey">>, <<"Avalanche">>, 
            <<"Dal">>, <<"Emerald">>, <<"Chandra">>, <<"Nainital">>, 
            <<"Sheshnag">>, <<"Suraj">>, <<"Moriri">>, <<"Tsongmo">>, 
            <<"Chilka">>, <<"Caspian">>, <<"Urmia">>, <<"Gavkhouni">>, 
            <<"Hamun">>, <<"Namak">>, <<"Bakhtegan">>, <<"Maharloo">>, 
            <<"Hamun">>, <<"Zarivar">>, <<"Habbaniyah">>, <<"Milh">>, 
            <<"Tharthar">>, <<"Sawa">>, <<"Alakol">>, <<"Balkhash">>, 
            <<"Chagan">>, <<"Kaindy">>, <<"Sasykkol">>, <<"Tengiz">>
    ]).

-define (wrandom(List), lists:nth(random:uniform(length(List)), List)).

gen_phrase_name() ->
    random:seed(erlang:timestamp()),
    W1 = ?wrandom(?WOODS),
    W2 = ?wrandom(?METALS),
    W3 = ?wrandom(?LAKES),
    <<W1/binary, <<"-">>/binary, W2/binary, <<"-">>/binary, W3/binary>>.

