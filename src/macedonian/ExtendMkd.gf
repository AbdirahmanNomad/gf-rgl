--# -path=.:../abstract:../common:prelude
concrete ExtendMkd of Extend = CatMkd ** open ResMkd in {

lin
  CompoundN n1 n2 = 
    let comp : Species => Number => Str
             = \\sp,n => case n1.relType of {
                           Pref   => n1.rel ! sp ! genNum n2.g n ++ n2.s ! sp ! n ;
                           AdjMod => n1.rel ! sp ! genNum n2.g n ++ n2.s ! Indef ! n ;
                           AdvMod => n2.s ! sp ! n ++ n1.rel ! sp ! genNum n2.g n
                         } ;
        voc : Number => Str
             = \\n => ""{-case n1.relType of {
                           Pref   => n1.rel ! Indef ! genNum n2.g n ++ n2.vocative ! n ;
                           AdjMod => n1.rel ! Indef ! genNum n2.g n ++ n2.vocative ! n ;
                           AdvMod => n2.vocative ! n ++ n1.rel ! Indef ! genNum n2.g n
                         }-}
    in {
         s   = comp ;
         count_form = comp ! Indef ! Pl ;
         vocative = voc ;
         rel = \\sp,n => "на" ++ comp ! sp ! Sg ;  relType = AdvMod ;
         g   = n2.g
    } ;

}

