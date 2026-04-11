concrete PhraseMkd of Phrase = CatMkd ** open Prelude, ResMkd in {
  lin NoPConj = {s : Str = []} ;
  lin NoVoc = {s : Str = []} ;
  lin PConjConj c = {s = c.s} ;
  lin PhrUtt pconj utt voc = {s : Str = pconj.s ++ utt.s ++ voc.s} ;
  lin UttAP ap = {s = ap.s ! Indef ! GSg Masc} ;
  lin UttAdv a = {s = a.s} ;
  lin UttCN cn = {s = cn.s ! Indef ! Sg} ;
  lin UttCard c = {s = c.s} ;
  lin UttIAdv i = {s = i.s} ;
  lin UttIP ip = {s = ip.s} ;
  lin UttImpPl p i = {s = p.s ++ i.s} ;
  lin UttImpPol p i = {s = p.s ++ i.s} ;
  lin UttImpSg p i = {s = p.s ++ i.s} ;
  lin UttInterj i = i ;
  lin UttNP np = {s = np.s ! RSubj} ;
  lin UttQS s = s ;
  lin UttS s = s ;
  lin UttVP vp = {s = vp.present ! Imperfective ! Sg ! P1} ;
  lin VocNP np = {s : Str = SOFT_BIND ++ "," ++ np.vocative} ;
}
