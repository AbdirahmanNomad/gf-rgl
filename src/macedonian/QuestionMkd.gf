concrete QuestionMkd of Question = CatMkd ** open Prelude,ResMkd in {
  lin AdvIAdv i a = {s = i.s ++ a.s} ;
  lin AdvIP ip a = {s = ip.s ++ a.s; g = ip.g} ;
  lin CompIAdv i = {s = i.s} ;
  lin CompIP ip = {s = ip.s} ;
  lin IdetCN i cn = {s = i.s ! cn.g ++ cn.s ! Indef ! i.n;
                     g = GSg Masc} ;
  lin IdetIP i = {s = i.s ! Masc; g = GSg Masc} ;
  lin IdetQuant i n = {s = \\g => i.s ! GSg Masc ++ n.s; n = Sg} ;
  lin PrepIP p ip = {s = p.s ++ ip.s} ;
  lincat QVP = {s : Str} ;
  lin QuestCl cl = {s = \\t,a,p => cl.s ! t ! a ! p ! Quest} ;
  lin QuestIAdv i cl = {s = \\t,a,p => i.s
                                         ++ cl.s ! t ! a ! p ! Main} ;
  lin QuestIComp i np = {s = \\t,a,p => i.s ++ np.s ! RSubj} ;
  lin QuestSlash ip c = {s = \\t,a,p => ip.s ++ c.s} ;
  lin QuestVP ip vp = {s = \\t,a,p => ip.s
                                        ++ vp.present ! Imperfective ! Sg ! P1} ;
}
