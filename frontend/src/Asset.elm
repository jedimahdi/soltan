module Asset exposing (..)

import Element exposing (Attribute, Element)
import Element.Background as Background


type Image
    = Image String


backgroundImage : Image -> Attribute msg
backgroundImage (Image url) =
    Background.image url


address : Image -> String
address (Image url) =
    url


img :
    List (Attribute msg)
    ->
        { src : Image
        , description : String
        }
    -> Element msg
img attrs data =
    Element.image attrs { src = address data.src, description = data.description }


image : String -> Image
image filename =
    Image ("/assets/images/" ++ filename)


clubs2 : Image
clubs2 =
    image "cards/clubs/x2/clubs_2.png"


clubs3 : Image
clubs3 =
    image "cards/clubs/x2/clubs_3.png"


clubs4 : Image
clubs4 =
    image "cards/clubs/x2/clubs_4.png"


clubs5 : Image
clubs5 =
    image "cards/clubs/x2/clubs_5.png"


clubs6 : Image
clubs6 =
    image "cards/clubs/x2/clubs_6.png"


clubs7 : Image
clubs7 =
    image "cards/clubs/x2/clubs_7.png"


clubs8 : Image
clubs8 =
    image "cards/clubs/x2/clubs_8.png"


clubs9 : Image
clubs9 =
    image "cards/clubs/x2/clubs_9.png"


clubs10 : Image
clubs10 =
    image "cards/clubs/x2/clubs_10.png"


clubsj : Image
clubsj =
    image "cards/clubs/x2/clubs_j.png"


clubsq : Image
clubsq =
    image "cards/clubs/x2/clubs_q.png"


clubsk : Image
clubsk =
    image "cards/clubs/x2/clubs_k.png"


clubsa : Image
clubsa =
    image "cards/clubs/x2/clubs_a.png"


diamonds2 : Image
diamonds2 =
    image "cards/diamonds/x2/diamonds_2.png"


diamonds3 : Image
diamonds3 =
    image "cards/diamonds/x2/diamonds_3.png"


diamonds4 : Image
diamonds4 =
    image "cards/diamonds/x2/diamonds_4.png"


diamonds5 : Image
diamonds5 =
    image "cards/diamonds/x2/diamonds_5.png"


diamonds6 : Image
diamonds6 =
    image "cards/diamonds/x2/diamonds_6.png"


diamonds7 : Image
diamonds7 =
    image "cards/diamonds/x2/diamonds_7.png"


diamonds8 : Image
diamonds8 =
    image "cards/diamonds/x2/diamonds_8.png"


diamonds9 : Image
diamonds9 =
    image "cards/diamonds/x2/diamonds_9.png"


diamonds10 : Image
diamonds10 =
    image "cards/diamonds/x2/diamonds_10.png"


diamondsj : Image
diamondsj =
    image "cards/diamonds/x2/diamonds_j.png"


diamondsq : Image
diamondsq =
    image "cards/diamonds/x2/diamonds_q.png"


diamondsk : Image
diamondsk =
    image "cards/diamonds/x2/diamonds_k.png"


diamondsa : Image
diamondsa =
    image "cards/diamonds/x2/diamonds_a.png"


hearts2 : Image
hearts2 =
    image "cards/hearts/x2/hearts_2.png"


hearts3 : Image
hearts3 =
    image "cards/hearts/x2/hearts_3.png"


hearts4 : Image
hearts4 =
    image "cards/hearts/x2/hearts_4.png"


hearts5 : Image
hearts5 =
    image "cards/hearts/x2/hearts_5.png"


hearts6 : Image
hearts6 =
    image "cards/hearts/x2/hearts_6.png"


hearts7 : Image
hearts7 =
    image "cards/hearts/x2/hearts_7.png"


hearts8 : Image
hearts8 =
    image "cards/hearts/x2/hearts_8.png"


hearts9 : Image
hearts9 =
    image "cards/hearts/x2/hearts_9.png"


hearts10 : Image
hearts10 =
    image "cards/hearts/x2/hearts_10.png"


heartsj : Image
heartsj =
    image "cards/hearts/x2/hearts_j.png"


heartsq : Image
heartsq =
    image "cards/hearts/x2/hearts_q.png"


heartsk : Image
heartsk =
    image "cards/hearts/x2/hearts_k.png"


heartsa : Image
heartsa =
    image "cards/hearts/x2/hearts_a.png"


spades2 : Image
spades2 =
    image "cards/spades/x2/spades_2.png"


spades3 : Image
spades3 =
    image "cards/spades/x2/spades_3.png"


spades4 : Image
spades4 =
    image "cards/spades/x2/spades_4.png"


spades5 : Image
spades5 =
    image "cards/spades/x2/spades_5.png"


spades6 : Image
spades6 =
    image "cards/spades/x2/spades_6.png"


spades7 : Image
spades7 =
    image "cards/spades/x2/spades_7.png"


spades8 : Image
spades8 =
    image "cards/spades/x2/spades_8.png"


spades9 : Image
spades9 =
    image "cards/spades/x2/spades_9.png"


spades10 : Image
spades10 =
    image "cards/spades/x2/spades_10.png"


spadesj : Image
spadesj =
    image "cards/spades/x2/spades_j.png"


spadesq : Image
spadesq =
    image "cards/spades/x2/spades_q.png"


spadesk : Image
spadesk =
    image "cards/spades/x2/spades_k.png"


spadesa : Image
spadesa =
    image "cards/spades/x2/spades_a.png"


largeClubs2 : Image
largeClubs2 =
    image "cards/clubs/x4/clubs_2.png"


largeClubs3 : Image
largeClubs3 =
    image "cards/clubs/x4/clubs_3.png"


largeClubs4 : Image
largeClubs4 =
    image "cards/clubs/x4/clubs_4.png"


largeClubs5 : Image
largeClubs5 =
    image "cards/clubs/x4/clubs_5.png"


largeClubs6 : Image
largeClubs6 =
    image "cards/clubs/x4/clubs_6.png"


largeClubs7 : Image
largeClubs7 =
    image "cards/clubs/x4/clubs_7.png"


largeClubs8 : Image
largeClubs8 =
    image "cards/clubs/x4/clubs_8.png"


largeClubs9 : Image
largeClubs9 =
    image "cards/clubs/x4/clubs_9.png"


largeClubs10 : Image
largeClubs10 =
    image "cards/clubs/x4/clubs_10.png"


largeClubsj : Image
largeClubsj =
    image "cards/clubs/x4/clubs_j.png"


largeClubsq : Image
largeClubsq =
    image "cards/clubs/x4/clubs_q.png"


largeClubsk : Image
largeClubsk =
    image "cards/clubs/x4/clubs_k.png"


largeClubsa : Image
largeClubsa =
    image "cards/clubs/x4/clubs_a.png"


largeDiamonds2 : Image
largeDiamonds2 =
    image "cards/diamonds/x4/diamonds_2.png"


largeDiamonds3 : Image
largeDiamonds3 =
    image "cards/diamonds/x4/diamonds_3.png"


largeDiamonds4 : Image
largeDiamonds4 =
    image "cards/diamonds/x4/diamonds_4.png"


largeDiamonds5 : Image
largeDiamonds5 =
    image "cards/diamonds/x4/diamonds_5.png"


largeDiamonds6 : Image
largeDiamonds6 =
    image "cards/diamonds/x4/diamonds_6.png"


largeDiamonds7 : Image
largeDiamonds7 =
    image "cards/diamonds/x4/diamonds_7.png"


largeDiamonds8 : Image
largeDiamonds8 =
    image "cards/diamonds/x4/diamonds_8.png"


largeDiamonds9 : Image
largeDiamonds9 =
    image "cards/diamonds/x4/diamonds_9.png"


largeDiamonds10 : Image
largeDiamonds10 =
    image "cards/diamonds/x4/diamonds_10.png"


largeDiamondsj : Image
largeDiamondsj =
    image "cards/diamonds/x4/diamonds_j.png"


largeDiamondsq : Image
largeDiamondsq =
    image "cards/diamonds/x4/diamonds_q.png"


largeDiamondsk : Image
largeDiamondsk =
    image "cards/diamonds/x4/diamonds_k.png"


largeDiamondsa : Image
largeDiamondsa =
    image "cards/diamonds/x4/diamonds_a.png"


largeHearts2 : Image
largeHearts2 =
    image "cards/hearts/x4/hearts_2.png"


largeHearts3 : Image
largeHearts3 =
    image "cards/hearts/x4/hearts_3.png"


largeHearts4 : Image
largeHearts4 =
    image "cards/hearts/x4/hearts_4.png"


largeHearts5 : Image
largeHearts5 =
    image "cards/hearts/x4/hearts_5.png"


largeHearts6 : Image
largeHearts6 =
    image "cards/hearts/x4/hearts_6.png"


largeHearts7 : Image
largeHearts7 =
    image "cards/hearts/x4/hearts_7.png"


largeHearts8 : Image
largeHearts8 =
    image "cards/hearts/x4/hearts_8.png"


largeHearts9 : Image
largeHearts9 =
    image "cards/hearts/x4/hearts_9.png"


largeHearts10 : Image
largeHearts10 =
    image "cards/hearts/x4/hearts_10.png"


largeHeartsj : Image
largeHeartsj =
    image "cards/hearts/x4/hearts_j.png"


largeHeartsq : Image
largeHeartsq =
    image "cards/hearts/x4/hearts_q.png"


largeHeartsk : Image
largeHeartsk =
    image "cards/hearts/x4/hearts_k.png"


largeHeartsa : Image
largeHeartsa =
    image "cards/hearts/x4/hearts_a.png"


largeSpades2 : Image
largeSpades2 =
    image "cards/spades/x4/spades_2.png"


largeSpades3 : Image
largeSpades3 =
    image "cards/spades/x4/spades_3.png"


largeSpades4 : Image
largeSpades4 =
    image "cards/spades/x4/spades_4.png"


largeSpades5 : Image
largeSpades5 =
    image "cards/spades/x4/spades_5.png"


largeSpades6 : Image
largeSpades6 =
    image "cards/spades/x4/spades_6.png"


largeSpades7 : Image
largeSpades7 =
    image "cards/spades/x4/spades_7.png"


largeSpades8 : Image
largeSpades8 =
    image "cards/spades/x4/spades_8.png"


largeSpades9 : Image
largeSpades9 =
    image "cards/spades/x4/spades_9.png"


largeSpades10 : Image
largeSpades10 =
    image "cards/spades/x4/spades_10.png"


largeSpadesj : Image
largeSpadesj =
    image "cards/spades/x4/spades_j.png"


largeSpadesq : Image
largeSpadesq =
    image "cards/spades/x4/spades_q.png"


largeSpadesk : Image
largeSpadesk =
    image "cards/spades/x4/spades_k.png"


largeSpadesa : Image
largeSpadesa =
    image "cards/spades/x4/spades_a.png"
