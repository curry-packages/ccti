(declare-datatypes () ((Ordering lt eq gt)))
(declare-datatypes (a) ((Maybe nothing (just (just_1 a)))))
(declare-datatypes (a b) ((Either (left (left_1 a)) (right (right_1 b)))))
(declare-datatypes
 (a b c d e f g h i j k l m n o)
 ((Tuple15
   (tuple15 (tuple15_1 a) (tuple15_2 b) (tuple15_3 c) (tuple15_4 d) (tuple15_5
                                                                     e) (tuple15_6
                                                                         f) (tuple15_7
                                                                             g) (tuple15_8
                                                                                 h) (tuple15_9
                                                                                     i) (tuple15_10
                                                                                         j) (tuple15_11
                                                                                             k) (tuple15_12
                                                                                                 l) (tuple15_13
                                                                                                     m) (tuple15_14
                                                                                                         n) (tuple15_15
                                                                                                             o)))))
(declare-datatypes
 (a b c d e f g h i j k l m n)
 ((Tuple14
   (tuple14 (tuple14_1 a) (tuple14_2 b) (tuple14_3 c) (tuple14_4 d) (tuple14_5
                                                                     e) (tuple14_6
                                                                         f) (tuple14_7
                                                                             g) (tuple14_8
                                                                                 h) (tuple14_9
                                                                                     i) (tuple14_10
                                                                                         j) (tuple14_11
                                                                                             k) (tuple14_12
                                                                                                 l) (tuple14_13
                                                                                                     m) (tuple14_14
                                                                                                         n)))))
(declare-datatypes
 (a b c d e f g h i j k l m)
 ((Tuple13
   (tuple13 (tuple13_1 a) (tuple13_2 b) (tuple13_3 c) (tuple13_4 d) (tuple13_5
                                                                     e) (tuple13_6
                                                                         f) (tuple13_7
                                                                             g) (tuple13_8
                                                                                 h) (tuple13_9
                                                                                     i) (tuple13_10
                                                                                         j) (tuple13_11
                                                                                             k) (tuple13_12
                                                                                                 l) (tuple13_13
                                                                                                     m)))))
(declare-datatypes
 (a b c d e f g h i j k l)
 ((Tuple12
   (tuple12 (tuple12_1 a) (tuple12_2 b) (tuple12_3 c) (tuple12_4 d) (tuple12_5
                                                                     e) (tuple12_6
                                                                         f) (tuple12_7
                                                                             g) (tuple12_8
                                                                                 h) (tuple12_9
                                                                                     i) (tuple12_10
                                                                                         j) (tuple12_11
                                                                                             k) (tuple12_12
                                                                                                 l)))))
(declare-datatypes
 (a b c d e f g h i j k)
 ((Tuple11
   (tuple11 (tuple11_1 a) (tuple11_2 b) (tuple11_3 c) (tuple11_4 d) (tuple11_5
                                                                     e) (tuple11_6
                                                                         f) (tuple11_7
                                                                             g) (tuple11_8
                                                                                 h) (tuple11_9
                                                                                     i) (tuple11_10
                                                                                         j) (tuple11_11
                                                                                             k)))))
(declare-datatypes
 (a b c d e f g h i j)
 ((Tuple10
   (tuple10 (tuple10_1 a) (tuple10_2 b) (tuple10_3 c) (tuple10_4 d) (tuple10_5
                                                                     e) (tuple10_6
                                                                         f) (tuple10_7
                                                                             g) (tuple10_8
                                                                                 h) (tuple10_9
                                                                                     i) (tuple10_10
                                                                                         j)))))
(declare-datatypes
 (a b c d e f g h i)
 ((Tuple9
   (tuple9 (tuple9_1 a) (tuple9_2 b) (tuple9_3 c) (tuple9_4 d) (tuple9_5
                                                                e) (tuple9_6
                                                                    f) (tuple9_7
                                                                        g) (tuple9_8
                                                                            h) (tuple9_9
                                                                                i)))))
(declare-datatypes
 (a b c d e f g h)
 ((Tuple8
   (tuple8 (tuple8_1 a) (tuple8_2 b) (tuple8_3 c) (tuple8_4 d) (tuple8_5
                                                                e) (tuple8_6
                                                                    f) (tuple8_7
                                                                        g) (tuple8_8
                                                                            h)))))
(declare-datatypes
 (a b c d e f g)
 ((Tuple7
   (tuple7 (tuple7_1 a) (tuple7_2 b) (tuple7_3 c) (tuple7_4 d) (tuple7_5
                                                                e) (tuple7_6
                                                                    f) (tuple7_7
                                                                        g)))))
(declare-datatypes
 (a b c d e f)
 ((Tuple6
   (tuple6 (tuple6_1 a) (tuple6_2 b) (tuple6_3 c) (tuple6_4 d) (tuple6_5
                                                                e) (tuple6_6
                                                                    f)))))
(declare-datatypes
 (a b c d e)
 ((Tuple5
   (tuple5 (tuple5_1 a) (tuple5_2 b) (tuple5_3 c) (tuple5_4 d) (tuple5_5 e)))))
(declare-datatypes
 (a b c d)
 ((Tuple4 (tuple4 (tuple4_1 a) (tuple4_2 b) (tuple4_3 c) (tuple4_4 d)))))
(declare-datatypes
 (a b c)
 ((Tuple3 (tuple3 (tuple3_1 a) (tuple3_2 b) (tuple3_3 c)))))
(declare-datatypes (a b) ((Tuple2 (tuple2 (tuple2_1 a) (tuple2_2 b)))))
(declare-datatypes () ((Unit unit)))
(declare-datatypes () ((Peano z (s (s_1 Peano)))))

(declare-const xs (List Int))
(declare-const p Peano)
(declare-const q Peano)
;(declare-const x1 Int)
(assert (and (= xs (insert _ nil)) (not (= p z))))
(check-sat)
(get-model)
