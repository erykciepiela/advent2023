module Garden where

import Text.Parsec
import Data.Functor
import Data.Maybe
import Data.Function
import Control.Arrow
import Data.List (groupBy, sortOn)
import Utils (parsed)

-- >>> answer 2023 5 1 new1
-- 462648396
new1 input = let
    (seeds, maps) = input `parsed` inputParser
    mapsFunction = foldl1 (>>>) (mapFunction <$> maps)
    in minimum $ mapsFunction <$> seeds

-- >>> compactRanges [(1, 10)]
-- >>> compactRanges [(11, 2), (9, 3), (2, 5), (1, 10), (0, 1)]
-- >>> compactRanges [(11, 2), (9, 3), (2, 5), (1, 10), (0, 1)]
-- [(1,10)]
-- [(0,13)]
-- [(0,13)]
compactRanges :: (Ord a, Num a) => [(a, a)] -> [(a, a)]
compactRanges ranges = let
    compact a = fmap (\subranges -> (fst $ head subranges, maximum (subranges <&> (\(s, r) -> s + r)) - fst (head subranges))) $ groupBy (\(s1, r1) (s2, _) -> s2 <= s1 + r1) a
    in iterate compact (sortOn fst ranges) !! 20


mapFunction :: RuleSet -> Int -> Int
mapFunction map x = fromMaybe x $ mapMaybe (\(Rule dstStart srcStart range) -> if x >= srcStart && x < srcStart + range then Just (dstStart + x - srcStart) else Nothing) map & listToMaybe

mapInverse :: RuleSet -> Int -> [Int]
mapInverse map y = mapMaybe (\(Rule dstStart srcStart range) -> if y >= dstStart && y < dstStart + range then Just (srcStart + y - dstStart) else Nothing) map


-- >>> parse inputParser "" "seeds: 19 20 21\n\nfertilizer-to-water map:\n12 13 14\n1 2 3*\n\nwater-to-light map:\n88 18 7\n18 25 70"
-- Right ([19,20,21],[[Rule {dstStart = 1, srcStart = 2, range = 3},Rule {dstStart = 12, srcStart = 13, range = 14}],[Rule {dstStart = 18, srcStart = 25, range = 70},Rule {dstStart = 88, srcStart = 18, range = 7}]])
inputParser :: Parsec String u ([Int], [RuleSet])
inputParser = do
    string "seeds: "
    seeds <- (read <$> many1 digit) `sepBy` char ' '
    string "\n\n"
    maps <- ruleSetsParser
    pure (seeds, maps)

inputParserWithSeedRanges :: Parsec String u ([(Int, Int)], [RuleSet])
inputParserWithSeedRanges = do
    string "seeds: "
    seeds <- (do
        p2 <- read <$> many1 digit
        char ' '
        range <- read <$> many1 digit
        pure (p2, range)) `sepBy` char ' '
    string "\n\n"
    maps <- ruleSetsParser
    pure (seeds, maps)

-- >>> parse ruleSetsParser "" "fertilizer-to-water map:\n12 13 14\n1 2 3*\n\nwater-to-light map:\n88 18 7\n18 25 70"
-- Right [[Rule {dstStart = 1, srcStart = 2, range = 3},Rule {dstStart = 12, srcStart = 13, range = 14}],[Rule {dstStart = 18, srcStart = 25, range = 70},Rule {dstStart = 88, srcStart = 18, range = 7}]]
ruleSetsParser :: Parsec String u [RuleSet]
ruleSetsParser = ruleSetParser `sepBy` string "*\n\n"

-- >>> parse ruleSetParser "" "fertilizer-to-water map:\n12 13 14\n1 2 3*"
-- Right [Rule {dstStart = 1, srcStart = 2, range = 3},Rule {dstStart = 12, srcStart = 13, range = 14}]
ruleSetParser  :: Parsec String u RuleSet
ruleSetParser = do
    headerParser
    ruleParser `sepBy` char '\n' <&> sortOn dstStart 

-- >>> parse ruleParser "" "12 13 14"
-- Right (Rule {dstStart = 12, srcStart = 13, range = 14})
ruleParser :: Parsec String u Rule
ruleParser = do
    numbers <- (read <$> many1 digit) `sepBy` char ' '
    pure $ Rule (numbers !! 0) (numbers !! 1) (numbers !! 2)

-- >>> parse headerParser "" "fertilizer-to-water map:\n"
-- Right ()
headerParser :: Parsec String u ()
headerParser = do
    try (string "seed-to-soil") <|> try (string "soil-to-fertilizer") <|> try (string "fertilizer-to-water") <|> try (string "water-to-light") <|> try (string "light-to-temperature") <|> try (string "temperature-to-humidity") <|> string "humidity-to-location"
    string " map:\n"
    pure ()

-- >>> answer 2023 5 2 new2
-- [Rule {dstStart = 0, srcStart = -162599755, range = 149426673},Rule {dstStart = 0, srcStart = 3191956729, range = 455228905},Rule {dstStart = 0, srcStart = 2076389123, range = 106364072},Rule {dstStart = 0, srcStart = 2246271809, range = 606264721},Rule {dstStart = 0, srcStart = 2914087687, range = 802804012},Rule {dstStart = 0, srcStart = 1739255909, range = 809087378},Rule {dstStart = 0, srcStart = 3331947297, range = 1754925622},Rule {dstStart = 92760141, srcStart = 363893739, range = 1662165481},Rule {dstStart = 100332146, srcStart = 2998972226, range = 1654593476},Rule {dstStart = 106364072, srcStart = 2182753195, range = 499900649},Rule {dstStart = 106862826, srcStart = 3003690706, range = 1648062796},Rule {dstStart = 110580631, srcStart = 329763915, range = 1644344991},Rule {dstStart = 144710455, srcStart = 3373408154, range = 1610215167},Rule {dstStart = 149426673, srcStart = 1916405476, range = 132884084},Rule {dstStart = 165017790, srcStart = 2241406913, range = 441246931},Rule {dstStart = 190757551, srcStart = 209280407, range = 1406455070},Rule {dstStart = 209103905, srcStart = 785955161, range = 1545821717},Rule {dstStart = 209280407, srcStart = 2285669530, range = 396984314},Rule {dstStart = 226560784, srcStart = 2472832593, range = 1528364838},Rule {dstStart = 247363468, srcStart = 3144191348, range = 1507562154},Rule {dstStart = 250963029, srcStart = 1142644585, range = 282333341},Rule {dstStart = 251664023, srcStart = 3480361722, range = 1503261599},Rule {dstStart = 261406345, srcStart = 3748865572, range = 1493519277},Rule {dstStart = 282310757, srcStart = 2049289560, range = 429352106},Rule {dstStart = 283984863, srcStart = 3583585030, range = 1470940759},Rule {dstStart = 309410320, srcStart = 2076389123, range = 768428817},Rule {dstStart = 310752230, srcStart = 243114563, range = 2499744145},Rule {dstStart = 321415690, srcStart = 2182753195, range = 58653718},Rule {dstStart = 336385104, srcStart = 2833510990, range = 2447814695},Rule {dstStart = 342640189, srcStart = -97088269, range = 1424338614},Rule {dstStart = 380069408, srcStart = 2241406913, range = 44262617},Rule {dstStart = 397401582, srcStart = 760576019, range = 2030204410},Rule {dstStart = 409726333, srcStart = 0, range = 1484657829},Rule {dstStart = 409742466, srcStart = 2176721269, range = 3588513868},Rule {dstStart = 424332025, srcStart = 2852536530, range = 196539291},Rule {dstStart = 479293006, srcStart = 2246271809, range = 3518963328},Rule {dstStart = 479293006, srcStart = 2603509968, range = 3518963328},Rule {dstStart = 483883727, srcStart = 3324946924, range = 3514372607},Rule {dstStart = 492658190, srcStart = 971951196, range = 3505598144},Rule {dstStart = 494809338, srcStart = 353313230, range = 3503446996},Rule {dstStart = 498892555, srcStart = -8913571, range = 1268086248},Rule {dstStart = 500989478, srcStart = 564295961, range = 1529384905},Rule {dstStart = 524293914, srcStart = 1101593120, range = 3473962420},Rule {dstStart = 552787285, srcStart = 1359515306, range = 3445469049},Rule {dstStart = 568452082, srcStart = -100080383, range = 1198526721},Rule {dstStart = 584720380, srcStart = 2416793325, range = 3300951001},Rule {dstStart = 620871316, srcStart = 484454103, range = 318349909},Rule {dstStart = 645380801, srcStart = 1124673807, range = 3538378246},Rule {dstStart = 652840896, srcStart = 1203603268, range = 3530918151},Rule {dstStart = 663999152, srcStart = 0, range = 1592691388},Rule {dstStart = 667815878, srcStart = 1147108884, range = 3515943169},Rule {dstStart = 719212681, srcStart = 1821763210, range = 3464546366},Rule {dstStart = 727305635, srcStart = 2973577444, range = 600845277},Rule {dstStart = 742681934, srcStart = 69797365, range = 536467356},Rule {dstStart = 752700417, srcStart = 2998972226, range = 3431058630},Rule {dstStart = 758446483, srcStart = 1237739489, range = 3425312564},Rule {dstStart = 759231097, srcStart = 3005502906, range = 3424527950},Rule {dstStart = 772900770, srcStart = 3019172579, range = 3410858277},Rule {dstStart = 779248641, srcStart = 3716891699, range = 6283366},Rule {dstStart = 785532007, srcStart = 269485885, range = 539601493},Rule {dstStart = 809087378, srcStart = 2548343287, range = 945838244},Rule {dstStart = 819409491, srcStart = 787714866, range = 3364349556},Rule {dstStart = 842280446, srcStart = 2447947744, range = 3341478601},Rule {dstStart = 845970820, srcStart = 1088293956, range = 3337788227},Rule {dstStart = 848612454, srcStart = 2250862530, range = 3335146593},Rule {dstStart = 871428602, srcStart = 729932494, range = 3312330445},Rule {dstStart = 874469781, srcStart = 358423659, range = 444380353},Rule {dstStart = 890092371, srcStart = 4277949937, range = 42097049},Rule {dstStart = 896502554, srcStart = 3142774363, range = 431648358},Rule {dstStart = 907727477, srcStart = -42158690, range = 859251326},Rule {dstStart = 909927230, srcStart = 3156199039, range = 3273831817},Rule {dstStart = 910023376, srcStart = 3689675651, range = 3273735671},Rule {dstStart = 932189420, srcStart = 378017010, range = 1043389896},Rule {dstStart = 945220828, srcStart = 2590144784, range = 3238538219},Rule {dstStart = 953714890, srcStart = 896502554, range = 482807303},Rule {dstStart = 961540761, srcStart = -489996752, range = 805438042},Rule {dstStart = 964050457, srcStart = 3144191348, range = 130653542},Rule {dstStart = 988670724, srcStart = 1388136917, range = 3195088323},Rule {dstStart = 991738666, srcStart = 0, range = 3192020381},Rule {dstStart = 1000500225, srcStart = 2548343287, range = 268751759},Rule {dstStart = 1071107509, srcStart = 759231097, range = 3112651538},Rule {dstStart = 1077839137, srcStart = 2817095046, range = 677086485},Rule {dstStart = 1077839137, srcStart = 2844817940, range = 1732657238},Rule {dstStart = 1094703999, srcStart = 226093219, range = 151923791},Rule {dstStart = 1097831573, srcStart = 1081646455, range = 3085927474},Rule {dstStart = 1099115898, srcStart = 2866094701, range = 302794076},Rule {dstStart = 1115974248, srcStart = 1784686286, range = 120315882},Rule {dstStart = 1117778238, srcStart = 1921960020, range = 3065980809},Rule {dstStart = 1146121952, srcStart = 1950303734, range = 3037637095},Rule {dstStart = 1190958320, srcStart = 199219654, range = 2992800727},Rule {dstStart = 1207835030, srcStart = 1784686286, range = 306827870},Rule {dstStart = 1236290130, srcStart = 1998258162, range = 93255994},Rule {dstStart = 1259962903, srcStart = 268224237, range = 2923796144},Rule {dstStart = 1269251984, srcStart = 5054525789, range = 66697504},Rule {dstStart = 1270527957, srcStart = 326345092, range = 2913231090},Rule {dstStart = 1293599454, srcStart = 642189614, range = 2890159593},Rule {dstStart = 1313840821, srcStart = 1176023584, range = 2869918226},Rule {dstStart = 1328150912, srcStart = 1905002168, range = 93255994},Rule {dstStart = 1328150912, srcStart = 4815610139, range = 2670105422},Rule {dstStart = 1329546124, srcStart = 2091514156, range = 78028488},Rule {dstStart = 1332454428, srcStart = 199219654, range = 2851304619},Rule {dstStart = 1335949488, srcStart = 0, range = 1821623126},Rule {dstStart = 1341420505, srcStart = 1014938560, range = 2842338542},Rule {dstStart = 1358167403, srcStart = 3090806078, range = 2825591644},Rule {dstStart = 1379309857, srcStart = 4866769084, range = 2618946477},Rule {dstStart = 1393042818, srcStart = 3160021621, range = 2790716229},Rule {dstStart = 1401459011, srcStart = 810589376, range = 2782300036},Rule {dstStart = 1401909974, srcStart = 3168888777, range = 1408586401},Rule {dstStart = 1402088224, srcStart = 3019172579, range = 2781670823},Rule {dstStart = 1405746853, srcStart = 1213097246, range = 541828376},Rule {dstStart = 1407574612, srcStart = 906838121, range = 685853267},Rule {dstStart = 1423475871, srcStart = 1838596570, range = 2760283176},Rule {dstStart = 1424977926, srcStart = 3191956729, range = 190757551},Rule {dstStart = 1452409475, srcStart = 838257601, range = 2731349572},Rule {dstStart = 1478192138, srcStart = 3229160565, range = 2705566909},Rule {dstStart = 1488534374, srcStart = 1046202207, range = 2695224673},Rule {dstStart = 1493139015, srcStart = 1471031672, range = 2690620032},Rule {dstStart = 1523978622, srcStart = 465294601, range = 2659780425},Rule {dstStart = 1539114684, srcStart = 3160021621, range = 2644644363},Rule {dstStart = 1571533532, srcStart = 3338512335, range = 2612225515},Rule {dstStart = 1594559581, srcStart = 2844817940, range = 21276761},Rule {dstStart = 1615735477, srcStart = 3382714280, range = 1194760898},Rule {dstStart = 1615836342, srcStart = 3168888777, range = 23067952},Rule {dstStart = 1617552130, srcStart = -312026428, range = 149426673},Rule {dstStart = 1634152245, srcStart = 3401131048, range = 2549606802},Rule {dstStart = 1638904294, srcStart = 3382714280, range = 139190145},Rule {dstStart = 1700873635, srcStart = 2097781236, range = 2482885412},Rule {dstStart = 1708571521, srcStart = 399542480, range = 1115120420},Rule {dstStart = 1717605398, srcStart = 3338512335, range = 2466153649},Rule {dstStart = 1754772241, srcStart = 810589376, range = 2428986806},Rule {dstStart = 1773426653, srcStart = 3512682562, range = 2410332394},Rule {dstStart = 1774875044, srcStart = 1184005409, range = 2408884003},Rule {dstStart = 1778094439, srcStart = 1099115898, range = 655809724},Rule {dstStart = 1780224111, srcStart = 4466873825, range = 2403534936},Rule {dstStart = 1824085445, srcStart = 1682589337, range = 2359673602},Rule {dstStart = 1837712164, srcStart = 0, range = 2346046883},Rule {dstStart = 1839406523, srcStart = 381714229, range = 2344352524},Rule {dstStart = 1860703913, srcStart = 305742584, range = 2323055134},Rule {dstStart = 1873325002, srcStart = 2973577444, range = 169196919},Rule {dstStart = 1881306421, srcStart = 278789291, range = 2302452626},Rule {dstStart = 1894384162, srcStart = 2742858708, range = 39356837},Rule {dstStart = 1894384162, srcStart = 1484657829, range = 1484657829},Rule {dstStart = 1899014938, srcStart = 1773426653, range = 2284744109},Rule {dstStart = 1911435456, srcStart = 5315277344, range = 2272323591},Rule {dstStart = 1933740999, srcStart = 3402643122, range = 321448984},Rule {dstStart = 1962726423, srcStart = 909927230, range = 2221032624},Rule {dstStart = 1993324395, srcStart = 0, range = 2190434652},Rule {dstStart = 2042521921, srcStart = 4815610139, range = 51158945},Rule {dstStart = 2067737753, srcStart = 1785847171, range = 2116021294},Rule {dstStart = 2072629125, srcStart = 1124673807, range = 2111129922},Rule {dstStart = 2095064202, srcStart = 1682589337, range = 2088694845},Rule {dstStart = 2099789767, srcStart = 4804984355, range = 14950546},Rule {dstStart = 2100705169, srcStart = 1545650287, range = 2083053878},Rule {dstStart = 2114740313, srcStart = 2885069454, range = 1128137426},Rule {dstStart = 2115307878, srcStart = 441322644, range = 2068451169},Rule {dstStart = 2135952035, srcStart = 1726225702, range = 713901177},Rule {dstStart = 2188804057, srcStart = 1705125539, range = 1994954990},Rule {dstStart = 2255189983, srcStart = 3348429818, range = 443299955},Rule {dstStart = 2305441728, srcStart = 107380774, range = 1878317319},Rule {dstStart = 2377040954, srcStart = 2299207917, range = 1171072872},Rule {dstStart = 2425087809, srcStart = 1075606279, range = 1588119071},Rule {dstStart = 2439072067, srcStart = 3924483574, range = 16819723},Rule {dstStart = 2455891790, srcStart = 3791729773, range = 217211191},Rule {dstStart = 2500785470, srcStart = 2859072453, range = 1512421410},Rule {dstStart = 2507471274, srcStart = 3021419800, range = 1505735606},Rule {dstStart = 2526444831, srcStart = 2135952035, range = 1856169206},Rule {dstStart = 2573484127, srcStart = 3278401204, range = 1439722753},Rule {dstStart = 2581343101, srcStart = 2944517538, range = 1636291058},Rule {dstStart = 2588570007, srcStart = 3003690706, range = 1424636873},Rule {dstStart = 2608238358, srcStart = 2459541635, range = 1404968522},Rule {dstStart = 2612470320, srcStart = 5281325685, range = 26296576},Rule {dstStart = 2638766896, srcStart = 6506066850, range = 208693195},Rule {dstStart = 2683278944, srcStart = 864040264, range = 1329927936},Rule {dstStart = 2701101998, srcStart = 3278401204, range = 649471724},Rule {dstStart = 2742067948, srcStart = 1273800407, range = 2702946766},Rule {dstStart = 2798704149, srcStart = 3715747077, range = 156185660},Rule {dstStart = 2810496375, srcStart = 2742858708, range = 1181624866},Rule {dstStart = 2810496375, srcStart = 5307622261, range = 1198444589},Rule {dstStart = 2826742681, srcStart = 2551285626, range = 1506540852},Rule {dstStart = 2847460091, srcStart = 3211730218, range = 1005903941},Rule {dstStart = 2849853212, srcStart = 2782215545, range = 620427577},Rule {dstStart = 2864027062, srcStart = 1470573702, range = 2597337692},Rule {dstStart = 2926322020, srcStart = 3503621226, range = 212125851},Rule {dstStart = 2939103647, srcStart = 922829268, range = 3258335653},Rule {dstStart = 2954889809, srcStart = 3871932737, range = 55940191},Rule {dstStart = 2984159691, srcStart = 3862282814, range = 146658150},Rule {dstStart = 3010830000, srcStart = 3927872928, range = 207028280},Rule {dstStart = 3019002892, srcStart = 2773729385, range = 10470414},Rule {dstStart = 3029473306, srcStart = 2790780429, range = 153737109},Rule {dstStart = 3062476586, srcStart = 1358475140, range = 29661777},Rule {dstStart = 3092138363, srcStart = 409742466, range = 55552135},Rule {dstStart = 3115342240, srcStart = 2380515300, range = 1617741034},Rule {dstStart = 3138447871, srcStart = 3715747077, range = 212125851},Rule {dstStart = 3139431799, srcStart = -50198206, range = 52524930},Rule {dstStart = 3147690498, srcStart = 1633749175, range = 71376364},Rule {dstStart = 3183210415, srcStart = 4580808596, range = 77333137},Rule {dstStart = 3217858280, srcStart = 4338962336, range = 204061128},Rule {dstStart = 3219066862, srcStart = 706757563, range = 45942854},Rule {dstStart = 3260543552, srcStart = 2581343101, range = 1713624195},Rule {dstStart = 3265009716, srcStart = 1130086491, range = 143713916},Rule {dstStart = 3333471621, srcStart = 5717744326, range = 112584953},Rule {dstStart = 3350573722, srcStart = 3927872928, range = 615150536},Rule {dstStart = 3421919408, srcStart = 4543023464, range = 151832804},Rule {dstStart = 3426492603, srcStart = 3897126015, range = 320508144},Rule {dstStart = 3446056574, srcStart = 2749719529, range = 1248536805},Rule {dstStart = 3450098653, srcStart = 3473284228, range = 37900570},Rule {dstStart = 3487999223, srcStart = 4080968704, range = 155844998},Rule {dstStart = 3521487829, srcStart = 2221977524, range = 205628468},Rule {dstStart = 3557602002, srcStart = 4134901208, range = 204061128},Rule {dstStart = 3573752212, srcStart = 4694856268, range = 15413409},Rule {dstStart = 3581406499, srcStart = 5866788159, range = 149043833},Rule {dstStart = 3589165621, srcStart = 3062909078, range = 1070061393},Rule {dstStart = 3598718222, srcStart = 1484657829, range = 241567873},Rule {dstStart = 3643844221, srcStart = 3837685117, range = 243283587},Rule {dstStart = 3664704414, srcStart = 2573484127, range = 1544072935},Rule {dstStart = 3730450332, srcStart = 1985698093, range = 111208249},Rule {dstStart = 3792322285, srcStart = 4710269677, range = 6723091},Rule {dstStart = 3799045376, srcStart = 2932303839, range = 1207389723},Rule {dstStart = 3840286095, srcStart = 2440126879, range = 361877006},Rule {dstStart = 3841658581, srcStart = 2598644681, range = 1696322615},Rule {dstStart = 3885671381, srcStart = 5717744326, range = 149043833},Rule {dstStart = 3887127808, srcStart = 3552862348, range = 284822769},Rule {dstStart = 3929650615, srcStart = 4811611406, range = 60655096},Rule {dstStart = 3965724258, srcStart = 4543023464, range = 167246213},Rule {dstStart = 3990305711, srcStart = 2926322020, range = 1368645276},Rule {dstStart = 3992733429, srcStart = 2099789767, range = 2083969280},Rule {dstStart = 3996287530, srcStart = 3557602002, range = 676710198},Rule {dstStart = 4013206880, srcStart = 2193968200, range = 1329927936},Rule {dstStart = 4034715214, srcStart = 2215476534, range = 1308419602},Rule {dstStart = 4132970471, srcStart = 4710269677, range = 101341729},Rule {dstStart = 4139693562, srcStart = 2146369167, range = 2146369167},Rule {dstStart = 4156826320, srcStart = 2263882658, range = 55483377},Rule {dstStart = 4171950577, srcStart = 3450098653, range = 23185575},Rule {dstStart = 4195136152, srcStart = 3511184798, range = 41677550},Rule {dstStart = 4200348658, srcStart = 2146369167, range = 80082634},Rule {dstStart = 4202163101, srcStart = 2747292152, range = 464438066},Rule {dstStart = 4212309697, srcStart = 2319366035, range = 3053875},Rule {dstStart = 4215363572, srcStart = 2322419910, range = 69320997},Rule {dstStart = 4219776196, srcStart = 2226451801, range = 2066286533},Rule {dstStart = 4228600334, srcStart = 2361239030, range = 488614182},Rule {dstStart = 4234312200, srcStart = 2240987805, range = 2051750529},Rule {dstStart = 4236813702, srcStart = 4292227071, range = 2740225},Rule {dstStart = 4239553927, srcStart = 4236813702, range = 55413369},Rule {dstStart = 4245401761, srcStart = 2352458099, range = 64335226},Rule {dstStart = 4273458962, srcStart = 2193968200, range = 21508334},Rule {dstStart = 4280431292, srcStart = 2226451801, range = 14536004}]

-- Just (Rule {dstStart = 342640189, srcStart = -97088269, range = 1424338614})
-- Just (Rule {dstStart = 498892555, srcStart = -8913571, range = 3499363779})
-- Just (Rule {dstStart = 342640189, srcStart = -97088269, range = 2084965803})
new2 input = let
    (seedRanges, ruleSets) = input `parsed` inputParserWithSeedRanges
    seedRuleSet = sortOn dstStart (seedRanges <&> (\(start, range) -> Rule start (- range - 1) range))
    ruleSet = sortOn dstStart $ mergeRules (foldr1 mergeRules ruleSets) seedRuleSet
    -- ruleSet = foldr1 mergeRules ruleSets
    -- in ruleSet & find (\rule -> rule.srcStart < 0)
    in ruleSet

data Rule = Rule
    { dstStart :: Int
    , srcStart :: Int
    , range :: Int } deriving Show

type RuleSet = [Rule]

-- precondition: downstream rules sorted by srcStart, upstream rules sorted by destStart
-- >>> mergeRules [Rule 2 6 2] [Rule 5 0 2]
-- [Rule {dstStart = 2, srcStart = 1, range = 1},Rule {dstStart = 2, srcStart = 6, range = 2},Rule {dstStart = 3, srcStart = 7, range = 3},Rule {dstStart = 5, srcStart = 0, range = 1}]
-- >>> mergeRules [Rule 3 2 2] [Rule 1 0 2]
-- [Rule {dstStart = 1, srcStart = 0, range = 1},Rule {dstStart = 3, srcStart = 1, range = 1},Rule {dstStart = 3, srcStart = 2, range = 2}]

-- >>> mergeRules [Rule 0 1 2] [Rule 2 3 2]
-- [Rule {dstStart = 0, srcStart = 1, range = 1},Rule {dstStart = 1, srcStart = 3, range = 1},Rule {dstStart = 2, srcStart = 3, range = 1},Rule {dstStart = 2, srcStart = 4, range = 1}]

mergeRules :: RuleSet -> RuleSet -> RuleSet
mergeRules [] [] = []
mergeRules downstream [] = downstream
mergeRules [] upstream = upstream
mergeRules downstream upstream
    | (head downstream).srcStart < (head upstream).dstStart =
        let resultRange = (head upstream).dstStart - (head downstream).srcStart
            resultHead = Rule { dstStart = (head downstream).dstStart, srcStart = (head downstream).srcStart, range = resultRange}
            resultTail
                | resultRange < (head downstream).range = mergeRules (Rule { dstStart = (head downstream).dstStart + resultRange, srcStart = (head downstream).srcStart + resultRange, range = (head upstream).dstStart - (head downstream).srcStart } : tail downstream) upstream
                | otherwise = mergeRules (tail downstream) (tail upstream)
        in sortOn dstStart $ resultHead : resultTail
    | (head downstream).srcStart > (head upstream).dstStart =
        let resultRange = (head downstream).srcStart - (head upstream).dstStart
            resultHead = Rule { dstStart = (head upstream).dstStart, srcStart = (head upstream).srcStart, range = resultRange}
            resultTail
                | resultRange < (head upstream).range = mergeRules downstream (Rule { dstStart = (head upstream).dstStart + resultRange, srcStart = (head upstream).srcStart + resultRange, range = (head downstream).srcStart - (head upstream).dstStart } : tail upstream)
                | otherwise = mergeRules downstream (tail upstream)
        in sortOn dstStart $ resultHead : resultTail
    | otherwise =
        let rangeDiff = (head downstream).range - (head upstream).range
            resultRange = min (head downstream).range (head upstream).range
            resultHead = Rule { dstStart = (head downstream).dstStart, srcStart = (head upstream).srcStart, range = resultRange }
            resultTail
                | rangeDiff > 0 = mergeRules downstream (Rule { dstStart = (head downstream).dstStart + resultRange, srcStart = (head downstream).srcStart + resultRange, range = rangeDiff } : tail upstream)
                | rangeDiff < 0 = mergeRules (Rule { dstStart = (head downstream).dstStart + resultRange, srcStart = (head downstream).srcStart + resultRange, range = - rangeDiff } : tail downstream) upstream
                | otherwise = mergeRules (tail downstream) (tail upstream)
        in sortOn dstStart $ resultHead : resultTail
