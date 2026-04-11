concrete VerbMkd of Verb = CatMkd ** open Prelude,ResMkd in {

  lin AdVVP a vp = {present = \\a2,n,p => a.s
                                            ++ vp.present ! a2 ! n ! p;
                    aorist = \\n,p => a.s ++ vp.aorist ! n ! p;
                    imperfect = \\a2,n,p => a.s ++ vp.imperfect ! a2 ! n ! p;
                    imperative = \\a2,n => a.s ++ vp.imperative ! a2 ! n;
                    participle = {aorist = \\a2,g => a.s
                                                       ++ vp.participle.aorist ! a2 ! g;
                                  imperfect = \\a2,g => a.s ++ vp.participle.imperfect ! a2 ! g;
                                  perfect = \\a2 => a.s ++ vp.participle.perfect ! a2;
                                  adjectival = \\a2 => a.s ++ vp.participle.adjectival ! a2;
                                  adverbial = a.s ++ vp.participle.adverbial};
                    noun_from_verb = a.s ++ vp.noun_from_verb; vtype = vp.vtype;
                    compl = \\v => a.s ++ vp.compl ! {g = GSg Masc; p = P1}} ;
  lin AdVVPSlash a v = {present = \\a2,n,p => a.s
                                                ++ v.present ! a2 ! n ! p;
                        aorist = \\n,p => a.s ++ v.aorist ! n ! p;
                        imperfect = \\a2,n,p => a.s ++ v.imperfect ! a2 ! n ! p;
                        imperative = \\a2,n => a.s ++ v.imperative ! a2 ! n;
                        participle = {aorist = \\a2,g => a.s
                                                           ++ v.participle.aorist ! a2 ! g;
                                      imperfect = \\a2,g => a.s ++ v.participle.imperfect ! a2 ! g;
                                      perfect = \\a2 => a.s ++ v.participle.perfect ! a2;
                                      adjectival = \\a2 => a.s ++ v.participle.adjectival ! a2;
                                      adverbial = a.s ++ v.participle.adverbial};
                        noun_from_verb = a.s ++ v.noun_from_verb; vtype = v.vtype;
                        compl = \\v2 => a.s ++ v.compl ! {g = GSg Masc; p = P1};
                        c2 = {s = a.s ++ v.c2.s; c = v.c2.c}} ;
  lin AdvVP vp a = {present = \\a2,n,p => vp.present ! a2 ! n ! p
                                            ++ a.s;
                    aorist = \\n,p => vp.aorist ! n ! p ++ a.s;
                    imperfect = \\a2,n,p => vp.imperfect ! a2 ! n ! p ++ a.s;
                    imperative = \\a2,n => vp.imperative ! a2 ! n ++ a.s;
                    participle = {aorist = \\a2,g => vp.participle.aorist ! a2 ! g
                                                       ++ a.s;
                                  imperfect = \\a2,g => vp.participle.imperfect ! a2 ! g ++ a.s;
                                  perfect = \\a2 => vp.participle.perfect ! a2 ++ a.s;
                                  adjectival = \\a2 => vp.participle.adjectival ! a2 ++ a.s;
                                  adverbial = vp.participle.adverbial ++ a.s};
                    noun_from_verb = vp.noun_from_verb ++ a.s; vtype = vp.vtype;
                    compl = \\v => vp.compl ! {g = GSg Masc; p = P1} ++ a.s} ;
  lin AdvVPSlash v a = {present = \\a2,n,p => v.present ! a2 ! n ! p
                                                ++ a.s;
                        aorist = \\n,p => v.aorist ! n ! p ++ a.s;
                        imperfect = \\a2,n,p => v.imperfect ! a2 ! n ! p ++ a.s;
                        imperative = \\a2,n => v.imperative ! a2 ! n ++ a.s;
                        participle = {aorist = \\a2,g => v.participle.aorist ! a2 ! g
                                                           ++ a.s;
                                      imperfect = \\a2,g => v.participle.imperfect ! a2 ! g ++ a.s;
                                      perfect = \\a2 => v.participle.perfect ! a2 ++ a.s;
                                      adjectival = \\a2 => v.participle.adjectival ! a2 ++ a.s;
                                      adverbial = v.participle.adverbial ++ a.s};
                        noun_from_verb = v.noun_from_verb ++ a.s; vtype = v.vtype;
                        compl = \\v2 => v.compl ! {g = GSg Masc; p = P1} ++ a.s;
                        c2 = {s = v.c2.s ++ a.s; c = v.c2.c}} ;
  lin CompAP ap = {s = ap.s ! Indef ! GSg Masc} ;
  lin CompAdv a = {s = a.s} ;
  lin CompCN cn = {s = cn.s ! Indef ! Sg} ;
  lin CompNP np = {s = np.s ! RSubj} ;
  lin ComplSlash vp np = vp ** {compl = \\a => vp.compl ! a ++ vp.c2.s ++ np.s ! RObj vp.c2.c} ;
  lin ComplVA va ap = {present = \\a,n,p => va.present ! a ! n ! p
                                              ++ ap.s ! Indef ! GSg Masc;
                       aorist = \\n,p => va.aorist ! n ! p ++ ap.s ! Indef ! GSg Masc;
                       imperfect = \\a,n,p => va.imperfect ! a ! n ! p
                                                ++ ap.s ! Indef ! GSg Masc;
                       imperative = \\a,n => va.imperative ! a ! n
                                               ++ ap.s ! Indef ! GSg Masc;
                       participle = {aorist = \\a,g => va.participle.aorist ! a ! g
                                                         ++ ap.s ! Indef ! g;
                                     imperfect = \\a,g => va.participle.imperfect ! a ! g
                                                            ++ ap.s ! Indef ! g;
                                     perfect = \\a => va.participle.perfect ! a
                                                        ++ ap.s ! Indef ! GSg Masc;
                                     adjectival = \\a => va.participle.adjectival ! a
                                                           ++ ap.s ! Indef ! GSg Masc;
                                     adverbial = va.participle.adverbial
                                                   ++ ap.s ! Indef ! GSg Masc};
                       noun_from_verb = va.noun_from_verb ++ ap.s ! Indef ! GSg Masc;
                       vtype = va.vtype;
                       compl = \\v => va.present ! Imperfective ! Sg ! v.p
                                        ++ ap.s ! Indef ! v.g} ;
  lin ComplVQ vq qs = {present = \\a,n,p => vq.present ! a ! n ! p
                                              ++ qs.s;
                       aorist = \\n,p => vq.aorist ! n ! p ++ qs.s;
                       imperfect = \\a,n,p => vq.imperfect ! a ! n ! p ++ qs.s;
                       imperative = \\a,n => vq.imperative ! a ! n ++ qs.s;
                       participle = {aorist = \\a,g => vq.participle.aorist ! a ! g
                                                         ++ qs.s;
                                     imperfect = \\a,g => vq.participle.imperfect ! a ! g ++ qs.s;
                                     perfect = \\a => vq.participle.perfect ! a ++ qs.s;
                                     adjectival = \\a => vq.participle.adjectival ! a ++ qs.s;
                                     adverbial = vq.participle.adverbial ++ qs.s};
                       noun_from_verb = vq.noun_from_verb ++ qs.s; vtype = vq.vtype;
                       compl = \\v => vq.present ! Imperfective ! Sg ! v.p ++ qs.s} ;
  lin ComplVS vs s = {present = \\a,n,p => vs.present ! a ! n ! p
                                             ++ s.s;
                      aorist = \\n,p => vs.aorist ! n ! p ++ s.s;
                      imperfect = \\a,n,p => vs.imperfect ! a ! n ! p ++ s.s;
                      imperative = \\a,n => vs.imperative ! a ! n ++ s.s;
                      participle = {aorist = \\a,g => vs.participle.aorist ! a ! g
                                                        ++ s.s;
                                    imperfect = \\a,g => vs.participle.imperfect ! a ! g ++ s.s;
                                    perfect = \\a => vs.participle.perfect ! a ++ s.s;
                                    adjectival = \\a => vs.participle.adjectival ! a ++ s.s;
                                    adverbial = vs.participle.adverbial ++ s.s};
                      noun_from_verb = vs.noun_from_verb ++ s.s; vtype = vs.vtype;
                      compl = \\v => vs.present ! Imperfective ! Sg ! v.p ++ s.s} ;
  lin ComplVV vv vp = {present = \\a,n,p => vv.present ! a ! n ! p
                                              ++ vp.present ! a ! n ! p;
                       aorist = \\n,p => vv.aorist ! n ! p ++ vp.aorist ! n ! p;
                       imperfect = \\a,n,p => vv.imperfect ! a ! n ! p
                                                ++ vp.imperfect ! a ! n ! p;
                       imperative = \\a,n => vv.imperative ! a ! n
                                               ++ vp.imperative ! a ! n;
                       participle = {aorist = \\a,g => vv.participle.aorist ! a ! g
                                                         ++ vp.participle.aorist ! a ! g;
                                     imperfect = \\a,g => vv.participle.imperfect ! a ! g
                                                            ++ vp.participle.imperfect ! a ! g;
                                     perfect = \\a => vv.participle.perfect ! a
                                                        ++ vp.participle.perfect ! a;
                                     adjectival = \\a => vv.participle.adjectival ! a
                                                           ++ vp.participle.adjectival ! a;
                                     adverbial = vv.participle.adverbial
                                                   ++ vp.participle.adverbial};
                       noun_from_verb = vv.noun_from_verb ++ vp.noun_from_verb;
                       vtype = vp.vtype;
                       compl = \\v => vv.present ! Imperfective ! Sg ! v.p
                                        ++ vp.compl ! {g = GSg Masc; p = P1}} ;
  lin ExtAdvVP vp a = {present = \\a2,n,p => vp.present ! a2 ! n ! p
                                               ++ a.s;
                       aorist = \\n,p => vp.aorist ! n ! p ++ a.s;
                       imperfect = \\a2,n,p => vp.imperfect ! a2 ! n ! p ++ a.s;
                       imperative = \\a2,n => vp.imperative ! a2 ! n ++ a.s;
                       participle = {aorist = \\a2,g => vp.participle.aorist ! a2 ! g
                                                          ++ a.s;
                                     imperfect = \\a2,g => vp.participle.imperfect ! a2 ! g ++ a.s;
                                     perfect = \\a2 => vp.participle.perfect ! a2 ++ a.s;
                                     adjectival = \\a2 => vp.participle.adjectival ! a2 ++ a.s;
                                     adverbial = vp.participle.adverbial ++ a.s};
                       noun_from_verb = vp.noun_from_verb ++ a.s; vtype = vp.vtype;
                       compl = \\v => vp.compl ! {g = GSg Masc; p = P1} ++ a.s} ;
  lin PassV2 v2 = {present = \\a,n,p => v2.present ! a ! n ! p;
                   aorist = \\n,p => v2.aorist ! n ! p;
                   imperfect = \\a,n,p => v2.imperfect ! a ! n ! p;
                   imperative = \\a,n => v2.imperative ! a ! n;
                   participle = {aorist = \\a,g => v2.participle.aorist ! a ! g;
                                 imperfect = \\a,g => v2.participle.imperfect ! a ! g;
                                 perfect = \\a => v2.participle.perfect ! a;
                                 adjectival = \\a => v2.participle.adjectival ! a;
                                 adverbial = v2.participle.adverbial};
                   noun_from_verb = v2.noun_from_verb; vtype = v2.vtype;
                   compl = \\v => v2.present ! Imperfective ! Sg ! v.p} ;
  lin ReflVP v = {present = \\a,n,p => v.present ! a ! n ! p;
                  aorist = \\n,p => v.aorist ! n ! p;
                  imperfect = \\a,n,p => v.imperfect ! a ! n ! p;
                  imperative = \\a,n => v.imperative ! a ! n;
                  participle = {aorist = \\a,g => v.participle.aorist ! a ! g;
                                imperfect = \\a,g => v.participle.imperfect ! a ! g;
                                perfect = \\a => v.participle.perfect ! a;
                                adjectival = \\a => v.participle.adjectival ! a;
                                adverbial = v.participle.adverbial};
                  noun_from_verb = v.noun_from_verb; vtype = v.vtype;
                  compl = \\v2 => v.compl ! {g = GSg Masc; p = P1}} ;
  lin Slash2V3 v3 np = {present = \\a,n,p => v3.present ! a ! n ! p
                                               ++ np.s ! RSubj;
                        aorist = \\n,p => v3.aorist ! n ! p ++ np.s ! RSubj;
                        imperfect = \\a,n,p => v3.imperfect ! a ! n ! p ++ np.s ! RSubj;
                        imperative = \\a,n => v3.imperative ! a ! n ++ np.s ! RSubj;
                        participle = {aorist = \\a,g => v3.participle.aorist ! a ! g
                                                          ++ np.s ! RSubj;
                                      imperfect = \\a,g => v3.participle.imperfect ! a ! g
                                                             ++ np.s ! RSubj;
                                      perfect = \\a => v3.participle.perfect ! a ++ np.s ! RSubj;
                                      adjectival = \\a => v3.participle.adjectival ! a
                                                            ++ np.s ! RSubj;
                                      adverbial = v3.participle.adverbial ++ np.s ! RSubj};
                        noun_from_verb = v3.noun_from_verb ++ np.s ! RSubj;
                        vtype = v3.vtype;
                        compl = \\v => v3.present ! Imperfective ! Sg ! v.p
                                         ++ np.s ! RSubj;
                        c2 = {s = v3.c2.s ++ np.s ! RSubj; c = v3.c2.c}} ;
  lin Slash3V3 v3 np = {present = \\a,n,p => v3.present ! a ! n ! p
                                               ++ np.s ! RSubj;
                        aorist = \\n,p => v3.aorist ! n ! p ++ np.s ! RSubj;
                        imperfect = \\a,n,p => v3.imperfect ! a ! n ! p ++ np.s ! RSubj;
                        imperative = \\a,n => v3.imperative ! a ! n ++ np.s ! RSubj;
                        participle = {aorist = \\a,g => v3.participle.aorist ! a ! g
                                                          ++ np.s ! RSubj;
                                      imperfect = \\a,g => v3.participle.imperfect ! a ! g
                                                             ++ np.s ! RSubj;
                                      perfect = \\a => v3.participle.perfect ! a ++ np.s ! RSubj;
                                      adjectival = \\a => v3.participle.adjectival ! a
                                                            ++ np.s ! RSubj;
                                      adverbial = v3.participle.adverbial ++ np.s ! RSubj};
                        noun_from_verb = v3.noun_from_verb ++ np.s ! RSubj;
                        vtype = v3.vtype;
                        compl = \\v => v3.present ! Imperfective ! Sg ! v.p
                                         ++ np.s ! RSubj;
                        c2 = {s = v3.c2.s ++ np.s ! RSubj; c = v3.c2.c}} ;
  lin SlashV2A v ap = {present = \\a,n,p => v.present ! a ! n ! p
                                              ++ ap.s ! Indef ! GSg Masc;
                       aorist = \\n,p => v.aorist ! n ! p ++ ap.s ! Indef ! GSg Masc;
                       imperfect = \\a,n,p => v.imperfect ! a ! n ! p
                                                ++ ap.s ! Indef ! GSg Masc;
                       imperative = \\a,n => v.imperative ! a ! n
                                               ++ ap.s ! Indef ! GSg Masc;
                       participle = {aorist = \\a,g => v.participle.aorist ! a ! g
                                                         ++ ap.s ! Indef ! g;
                                     imperfect = \\a,g => v.participle.imperfect ! a ! g
                                                            ++ ap.s ! Indef ! g;
                                     perfect = \\a => v.participle.perfect ! a
                                                        ++ ap.s ! Indef ! GSg Masc;
                                     adjectival = \\a => v.participle.adjectival ! a
                                                           ++ ap.s ! Indef ! GSg Masc;
                                     adverbial = v.participle.adverbial ++ ap.s ! Indef ! GSg Masc};
                       noun_from_verb = v.noun_from_verb ++ ap.s ! Indef ! GSg Masc;
                       vtype = v.vtype;
                       compl = \\v2 => v.present ! Imperfective ! Sg ! v2.p
                                         ++ ap.s ! Indef ! v2.g;
                       c2 = {s = v.c2.s ++ ap.s ! Indef ! GSg Masc; c = v.c2.c}} ;
  lin SlashV2Q v qs = {present = \\a,n,p => v.present ! a ! n ! p
                                              ++ qs.s;
                       aorist = \\n,p => v.aorist ! n ! p ++ qs.s;
                       imperfect = \\a,n,p => v.imperfect ! a ! n ! p ++ qs.s;
                       imperative = \\a,n => v.imperative ! a ! n ++ qs.s;
                       participle = {aorist = \\a,g => v.participle.aorist ! a ! g
                                                         ++ qs.s;
                                     imperfect = \\a,g => v.participle.imperfect ! a ! g ++ qs.s;
                                     perfect = \\a => v.participle.perfect ! a ++ qs.s;
                                     adjectival = \\a => v.participle.adjectival ! a ++ qs.s;
                                     adverbial = v.participle.adverbial ++ qs.s};
                       noun_from_verb = v.noun_from_verb ++ qs.s; vtype = v.vtype;
                       compl = \\v2 => v.present ! Imperfective ! Sg ! v2.p ++ qs.s;
                       c2 = {s = v.c2.s ++ qs.s; c = v.c2.c}} ;
  lin SlashV2S v s = {present = \\a,n,p => v.present ! a ! n ! p
                                             ++ s.s;
                      aorist = \\n,p => v.aorist ! n ! p ++ s.s;
                      imperfect = \\a,n,p => v.imperfect ! a ! n ! p ++ s.s;
                      imperative = \\a,n => v.imperative ! a ! n ++ s.s;
                      participle = {aorist = \\a,g => v.participle.aorist ! a ! g ++ s.s;
                                    imperfect = \\a,g => v.participle.imperfect ! a ! g ++ s.s;
                                    perfect = \\a => v.participle.perfect ! a ++ s.s;
                                    adjectival = \\a => v.participle.adjectival ! a ++ s.s;
                                    adverbial = v.participle.adverbial ++ s.s};
                      noun_from_verb = v.noun_from_verb ++ s.s; vtype = v.vtype;
                      compl = \\v2 => v.present ! Imperfective ! Sg ! v2.p ++ s.s;
                      c2 = {s = v.c2.s ++ s.s; c = v.c2.c}} ;
  lin SlashV2V v vp = {present = \\a,n,p => v.present ! a ! n ! p
                                              ++ vp.present ! a ! n ! p;
                       aorist = \\n,p => v.aorist ! n ! p ++ vp.aorist ! n ! p;
                       imperfect = \\a,n,p => v.imperfect ! a ! n ! p
                                                ++ vp.imperfect ! a ! n ! p;
                       imperative = \\a,n => v.imperative ! a ! n
                                               ++ vp.imperative ! a ! n;
                       participle = {aorist = \\a,g => v.participle.aorist ! a ! g
                                                         ++ vp.participle.aorist ! a ! g;
                                     imperfect = \\a,g => v.participle.imperfect ! a ! g
                                                            ++ vp.participle.imperfect ! a ! g;
                                     perfect = \\a => v.participle.perfect ! a
                                                        ++ vp.participle.perfect ! a;
                                     adjectival = \\a => v.participle.adjectival ! a
                                                           ++ vp.participle.adjectival ! a;
                                     adverbial = v.participle.adverbial ++ vp.participle.adverbial};
                       noun_from_verb = v.noun_from_verb ++ vp.noun_from_verb;
                       vtype = vp.vtype;
                       compl = \\v2 => v.present ! Imperfective ! Sg ! v2.p
                                         ++ vp.compl ! {g = GSg Masc; p = P1};
                       c2 = {s = v.c2.s ++ vp.present ! Imperfective ! Sg ! P1;
                             c = v.c2.c}} ;
  lin SlashV2VNP v np v2 = {present = \\a,n,p => v.present ! a ! n
                                                   ! p
                                                   ++ np.s ! RSubj ++ v2.present ! a ! n ! p;
                            aorist = \\n,p => v.aorist ! n ! p
                                                ++ np.s ! RSubj ++ v2.aorist ! n ! p;
                            imperfect = \\a,n,p => v.imperfect ! a ! n ! p
                                                     ++ np.s ! RSubj ++ v2.imperfect ! a ! n ! p;
                            imperative = \\a,n => v.imperative ! a ! n
                                                    ++ np.s ! RSubj ++ v2.imperative ! a ! n;
                            participle = {aorist = \\a,g => v.participle.aorist ! a ! g
                                                              ++ np.s ! RSubj
                                                                   ++ v2.participle.aorist ! a ! g;
                                          imperfect = \\a,g => v.participle.imperfect ! a ! g
                                                                 ++ np.s ! RSubj
                                                                      ++ v2.participle.imperfect ! a
                                                                           ! g;
                                          perfect = \\a => v.participle.perfect ! a
                                                             ++ np.s ! RSubj
                                                                  ++ v2.participle.perfect ! a;
                                          adjectival = \\a => v.participle.adjectival ! a
                                                                ++ np.s ! RSubj
                                                                     ++ v2.participle.adjectival
                                                                          ! a;
                                          adverbial = v.participle.adverbial
                                                        ++ np.s ! RSubj ++ v2.participle.adverbial};
                            noun_from_verb = v.noun_from_verb
                                               ++ np.s ! RSubj ++ v2.noun_from_verb;
                            vtype = v2.vtype;
                            compl = \\v3 => v.present ! Imperfective ! Sg ! v3.p
                                              ++ np.s ! RSubj ++ v2.compl ! {g = GSg Masc; p = P1};
                            c2 = {s = v.c2.s ++ np.s ! RSubj ++ v2.c2.s; c = v2.c2.c}} ;
  lin SlashV2a v = v ** {compl = \\_ => []} ;
  lin SlashVV vv v = {present = \\a,n,p => vv.present ! a ! n ! p
                                             ++ v.present ! a ! n ! p;
                      aorist = \\n,p => vv.aorist ! n ! p ++ v.aorist ! n ! p;
                      imperfect = \\a,n,p => vv.imperfect ! a ! n ! p
                                               ++ v.imperfect ! a ! n ! p;
                      imperative = \\a,n => vv.imperative ! a ! n
                                              ++ v.imperative ! a ! n;
                      participle = {aorist = \\a,g => vv.participle.aorist ! a ! g
                                                        ++ v.participle.aorist ! a ! g;
                                    imperfect = \\a,g => vv.participle.imperfect ! a ! g
                                                           ++ v.participle.imperfect ! a ! g;
                                    perfect = \\a => vv.participle.perfect ! a
                                                       ++ v.participle.perfect ! a;
                                    adjectival = \\a => vv.participle.adjectival ! a
                                                          ++ v.participle.adjectival ! a;
                                    adverbial = vv.participle.adverbial ++ v.participle.adverbial};
                      noun_from_verb = vv.noun_from_verb ++ v.noun_from_verb;
                      vtype = v.vtype;
                      compl = \\v2 => vv.present ! Imperfective ! Sg ! v2.p
                                        ++ v.compl ! {g = GSg Masc; p = P1};
                      c2 = {s = vv.present ! Imperfective ! Sg ! P1 ++ v.c2.s;
                            c = v.c2.c}} ;
  lin UseComp c = {present = \\a,n,p => c.s; aorist = \\n,p => c.s;
                   imperfect = \\a,n,p => c.s; imperative = \\a,n => c.s;
                   participle = {aorist = \\a,g => c.s; imperfect = \\a,g => c.s;
                                 perfect = \\a => c.s; adjectival = \\a => c.s; adverbial = c.s};
                   noun_from_verb = c.s; vtype = VNormal; compl = \\v => c.s} ;
  lin UseCopula = {present = \\a,n,p => []; aorist = \\n,p => [];
                   imperfect = \\a,n,p => []; imperative = \\a,n => [];
                   participle = {aorist = \\a,g => []; imperfect = \\a,g => [];
                                 perfect = \\a => []; adjectival = \\a => []; adverbial = []};
                   noun_from_verb = []; vtype = VNormal; compl = \\v => []} ;
  lin UseV v = v ** {compl = \\_ => []} ;
  lin VPSlashPrep vp p = {present = \\a,n,p2 => vp.present ! a ! n
                                                  ! p2
                                                  ++ p.s;
                          aorist = \\n,p2 => vp.aorist ! n ! p2 ++ p.s;
                          imperfect = \\a,n,p2 => vp.imperfect ! a ! n ! p2 ++ p.s;
                          imperative = \\a,n => vp.imperative ! a ! n ++ p.s;
                          participle = {aorist = \\a,g => vp.participle.aorist ! a ! g
                                                            ++ p.s;
                                        imperfect = \\a,g => vp.participle.imperfect ! a ! g ++ p.s;
                                        perfect = \\a => vp.participle.perfect ! a ++ p.s;
                                        adjectival = \\a => vp.participle.adjectival ! a ++ p.s;
                                        adverbial = vp.participle.adverbial ++ p.s};
                          noun_from_verb = vp.noun_from_verb ++ p.s; vtype = vp.vtype;
                          compl = \\v => vp.compl ! {g = GSg Masc; p = P1} ++ p.s;
                          c2 = {s = vp.present ! Imperfective ! Sg ! P1 ++ p.s; c = p.c}} ;
}
