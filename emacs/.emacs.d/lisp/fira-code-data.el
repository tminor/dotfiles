;; -*- coding: utf-8 -*-
(defconst fira-code--data
  '(["space.frac" " " "\xe100"]  ;   
    ["exclam_exclam.liga" "!!" "\xe101"]  ;   
    ["exclam_exclam_period.liga" "!!." "\xe102"]  ;   
    ["exclam_equal.liga" "!=" "\xe103"]  ;   
    ["exclam_equal_equal.liga" "!==" "\xe104"]  ;   
    ["numbersign_exclam.liga" "#!" "\xe105"]  ;   
    ["numbersign_numbersign.liga" "##" "\xe106"]  ;   
    ["numbersign_numbersign_numbersign.liga" "###" "\xe107"]  ;   
    ["numbersign_numbersign_numbersign_numbersign.liga" "####" "\xe108"]  ;   
    ["numbersign_parenleft.liga" "#(" "\xe109"]  ;   
    ["numbersign_colon.liga" "#:" "\xe10a"]  ;   
    ["numbersign_colon.liga_rem" "#:" "\xe10b"]  ;   
    ["numbersign_equal.liga" "#=" "\xe10c"]  ;   
    ["numbersign_question.liga" "#?" "\xe10d"]  ;   
    ["numbersign_bracketleft.liga" "#[" "\xe10e"]  ;   
    ["numbersign_underscore.liga" "#_" "\xe10f"]  ;   
    ["numbersign_underscore_parenleft.liga" "#_(" "\xe110"]  ;   
    ["numbersign_braceleft.liga" "#{" "\xe111"]  ;   
    ["dollar.ss05" "$" "\xe112"]  ;   
    ["dollar_greater.liga" "$>" "\xe113"]  ;   
    ["percent_percent.liga" "%%" "\xe114"]  ;   
    ["ampersand.ss03" "&" "\xe115"]  ;   
    ["ampersand_ampersand.liga" "&&" "\xe116"]  ;   
    ["parenleft.case" "(" "\xe117"]  ;   
    ["parenleft.dnom" "(" "\xe118"]  ;   
    ["parenleft.numr" "(" "\xe119"]  ;   
    ["parenright.case" ")" "\xe11a"]  ;   
    ["parenright.dnom" ")" "\xe11b"]  ;   
    ["parenright.numr" ")" "\xe11c"]  ;   
    ["asterisk.lc" "*" "\xe11d"]  ;   
    ["asterisk_asterisk.liga" "**" "\xe11e"]  ;   
    ["asterisk_asterisk_asterisk.liga" "***" "\xe11f"]  ;   
    ["asterisk_slash.liga" "*/" "\xe120"]  ;   
    ["asterisk_greater.liga" "*>" "\xe121"]  ;   
    ["plus.dnom" "+" "\xe122"]  ;   
    ["plus.lc" "+" "\xe123"]  ;   
    ["plus.numr" "+" "\xe124"]  ;   
    ["plus_plus.liga" "++" "\xe125"]  ;   
    ["plus_plus_plus.liga" "+++" "\xe126"]  ;   
    ["plus_greater.liga" "+>" "\xe127"]  ;   
    ["hyphen.case" "-" "\xe128"]  ;   
    ["hyphen.lc" "-" "\xe129"]  ;   
    ["hyphen_hyphen.liga" "--" "\xe12a"]  ;   
    ["hyphen_hyphen_hyphen.liga" "---" "\xe12b"]  ;   
    ["hyphen_hyphen_greater.liga" "-->" "\xe12c"]  ;   
    ["hyphen_less.liga" "-<" "\xe12d"]  ;   
    ["hyphen_less_less.liga" "-<<" "\xe12e"]  ;   
    ["hyphen_greater.liga" "->" "\xe12f"]  ;   
    ["hyphen_greater_greater.liga" "->>" "\xe130"]  ;   
    ["hyphen_bar.liga" "-|" "\xe131"]  ;   
    ["hyphen_asciitilde.liga" "-~" "\xe132"]  ;   
    ["period_hyphen.liga" ".-" "\xe133"]  ;   
    ["period_period.liga" ".." "\xe134"]  ;   
    ["period_period_period.liga" "..." "\xe135"]  ;   
    ["period_period_less.liga" "..<" "\xe136"]  ;   
    ["period_equal.liga" ".=" "\xe137"]  ;   
    ["period_question.liga" ".?" "\xe138"]  ;   
    ["slash_asterisk.liga" "/*" "\xe139"]  ;   
    ["slash_slash.liga" "//" "\xe13a"]  ;   
    ["slash_slash_slash.liga" "///" "\xe13b"]  ;   
    ["slash_equal.liga" "/=" "\xe13c"]  ;   
    ["slash_equal_equal.liga" "/==" "\xe13d"]  ;   
    ["slash_greater.liga" "/>" "\xe13e"]  ;   
    ["slash_backslash.liga" "/\\" "\xe13f"]  ;   
    ["zero.dnom" "0" "\xe140"]  ;   
    ["zero.numr" "0" "\xe141"]  ;   
    ["zero.tosf" "0" "\xe142"]  ;   
    ["zero.tosf.zero" "0" "\xe143"]  ;   
    ["zero.zero" "0" "\xe144"]  ;   
    ["zero.zero.tosf" "0" "\xe145"]  ;   
    ["one.dnom" "1" "\xe146"]  ;   
    ["one.numr" "1" "\xe147"]  ;   
    ["one.tosf" "1" "\xe148"]  ;   
    ["two.dnom" "2" "\xe149"]  ;   
    ["two.numr" "2" "\xe14a"]  ;   
    ["two.tosf" "2" "\xe14b"]  ;   
    ["three.dnom" "3" "\xe14c"]  ;   
    ["three.numr" "3" "\xe14d"]  ;   
    ["three.tosf" "3" "\xe14e"]  ;   
    ["four.dnom" "4" "\xe14f"]  ;   
    ["four.numr" "4" "\xe150"]  ;   
    ["four.tosf" "4" "\xe151"]  ;   
    ["five.dnom" "5" "\xe152"]  ;   
    ["five.numr" "5" "\xe153"]  ;   
    ["five.tosf" "5" "\xe154"]  ;   
    ["six.dnom" "6" "\xe155"]  ;   
    ["six.numr" "6" "\xe156"]  ;   
    ["six.tosf" "6" "\xe157"]  ;   
    ["seven.dnom" "7" "\xe158"]  ;   
    ["seven.numr" "7" "\xe159"]  ;   
    ["seven.tosf" "7" "\xe15a"]  ;   
    ["eight.dnom" "8" "\xe15b"]  ;   
    ["eight.numr" "8" "\xe15c"]  ;   
    ["eight.tosf" "8" "\xe15d"]  ;   
    ["nine.dnom" "9" "\xe15e"]  ;   
    ["nine.numr" "9" "\xe15f"]  ;   
    ["nine.tosf" "9" "\xe160"]  ;   
    ["colon.uc" ":" "\xe161"]  ;   
    ["colon_colon.liga" "::" "\xe162"]  ;   
    ["colon_colon_colon.liga" ":::" "\xe163"]  ;   
    ["colon_colon_equal.liga" "::=" "\xe164"]  ;   
    ["colon_less.liga" ":<" "\xe165"]  ;   
    ["colon_equal.liga" ":=" "\xe166"]  ;   
    ["colon_greater.liga" ":>" "\xe167"]  ;   
    ["semicolon_semicolon.liga" ";;" "\xe168"]  ;   
    ["less_exclam_hyphen_hyphen.liga" "<!--" "\xe169"]  ;   
    ["less_dollar.liga" "<$" "\xe16a"]  ;   
    ["less_dollar_greater.liga" "<$>" "\xe16b"]  ;   
    ["less_asterisk.liga" "<*" "\xe16c"]  ;   
    ["less_asterisk_greater.liga" "<*>" "\xe16d"]  ;   
    ["less_plus.liga" "<+" "\xe16e"]  ;   
    ["less_plus_greater.liga" "<+>" "\xe16f"]  ;   
    ["less_hyphen.liga" "<-" "\xe170"]  ;   
    ["less_hyphen_hyphen.liga" "<--" "\xe171"]  ;   
    ["less_hyphen_less.liga" "<-<" "\xe172"]  ;   
    ["less_hyphen_greater.liga" "<->" "\xe173"]  ;   
    ["less_hyphen_bar.liga" "<-|" "\xe174"]  ;   
    ["less_slash.liga" "</" "\xe175"]  ;   
    ["less_slash_greater.liga" "</>" "\xe176"]  ;   
    ["less_colon.liga" "<:" "\xe177"]  ;   
    ["less_less.liga" "<<" "\xe178"]  ;   
    ["less_less_hyphen.liga" "<<-" "\xe179"]  ;   
    ["less_less_less.liga" "<<<" "\xe17a"]  ;   
    ["less_less_equal.liga" "<<=" "\xe17b"]  ;   
    ["less_equal.liga" "<=" "\xe17c"]  ;   
    ["less_equal.ss02" "<=" "\xe17d"]  ;   
    ["less_equal_less.liga" "<=<" "\xe17e"]  ;   
    ["less_equal_equal.liga" "<==" "\xe17f"]  ;   
    ["less_equal_equal_greater.liga" "<==>" "\xe180"]  ;   
    ["less_equal_greater.liga" "<=>" "\xe181"]  ;   
    ["less_equal_bar.liga" "<=|" "\xe182"]  ;   
    ["less_greater.liga" "<>" "\xe183"]  ;   
    ["less_bar.liga" "<|" "\xe184"]  ;   
    ["less_bar_greater.liga" "<|>" "\xe185"]  ;   
    ["less_bar_bar.liga" "<||" "\xe186"]  ;   
    ["less_bar_bar_bar.liga" "<|||" "\xe187"]  ;   
    ["less_asciitilde.liga" "<~" "\xe188"]  ;   
    ["less_asciitilde_greater.liga" "<~>" "\xe189"]  ;   
    ["less_asciitilde_asciitilde.liga" "<~~" "\xe18a"]  ;   
    ["equal.dnom" "=" "\xe18b"]  ;   
    ["equal.numr" "=" "\xe18c"]  ;   
    ["equal_exclam_equal.liga" "=!=" "\xe18d"]  ;   
    ["equal_slash_equal.liga" "=/=" "\xe18e"]  ;   
    ["equal_colon_equal.liga" "=:=" "\xe18f"]  ;   
    ["equal_less_less.liga" "=<<" "\xe190"]  ;   
    ["equal_equal.liga" "==" "\xe191"]  ;   
    ["equal_equal_equal.liga" "===" "\xe192"]  ;   
    ["equal_equal_greater.liga" "==>" "\xe193"]  ;   
    ["equal_greater.liga" "=>" "\xe194"]  ;   
    ["equal_greater_greater.liga" "=>>" "\xe195"]  ;   
    ["greater_hyphen.liga" ">-" "\xe196"]  ;   
    ["greater_hyphen_greater.liga" ">->" "\xe197"]  ;   
    ["greater_colon.liga" ">:" "\xe198"]  ;   
    ["greater_equal.liga" ">=" "\xe199"]  ;   
    ["greater_equal.ss02" ">=" "\xe19a"]  ;   
    ["greater_equal_greater.liga" ">=>" "\xe19b"]  ;   
    ["greater_greater.liga" ">>" "\xe19c"]  ;   
    ["greater_greater_hyphen.liga" ">>-" "\xe19d"]  ;   
    ["greater_greater_equal.liga" ">>=" "\xe19e"]  ;   
    ["greater_greater_greater.liga" ">>>" "\xe19f"]  ;   
    ["question_period.liga" "?." "\xe1a0"]  ;   
    ["question_colon.liga" "?:" "\xe1a1"]  ;   
    ["question_equal.liga" "?=" "\xe1a2"]  ;   
    ["question_question.liga" "??" "\xe1a3"]  ;   
    ["at.ss06" "@" "\xe1a4"]  ;   
    ["F_l.liga" "Fl" "\xe1a5"]  ;   
    ["T_l.liga" "Tl" "\xe1a6"]  ;   
    ["bracketleft.case" "[" "\xe1a7"]  ;   
    ["bracketleft_bar.liga" "[|" "\xe1a8"]  ;   
    ["backslash.ss08" "\\" "\xe1a9"]  ;   
    ["backslash.thick.ss08" "\\" "\xe1aa"]  ;   
    ["backslash_slash.liga" "\\/" "\xe1ab"]  ;   
    ["bracketright.case" "]" "\xe1ac"]  ;   
    ["bracketright_numbersign.liga" "]#" "\xe1ad"]  ;   
    ["asciicircum_equal.liga" "^=" "\xe1ae"]  ;   
    ["underscore_underscore.liga" "__" "\xe1af"]  ;   
    ["underscore_bar_underscore.liga" "_|_" "\xe1b0"]  ;   
    ["grave.case" "`" "\xe1b1"]  ;   
    ["f_l.liga" "fl" "\xe1b2"]  ;   
    ["i.loclTRK" "i" "\xe1b3"]  ;   
    ["i.salt_low" "i" "\xe1b4"]  ;   
    ["j.salt_low" "j" "\xe1b5"]  ;   
    ["r.ss01" "r" "\xe1b6"]  ;   
    ["w_w_w.liga" "www" "\xe1b7"]  ;   
    ["x.multiply" "x" "\xe1b8"]  ;   
    ["braceleft.case" "{" "\xe1b9"]  ;   
    ["braceleft_bar.liga" "{|" "\xe1ba"]  ;   
    ["bar_hyphen.liga" "|-" "\xe1bb"]  ;   
    ["bar_hyphen_greater.liga" "|->" "\xe1bc"]  ;   
    ["bar_equal.liga" "|=" "\xe1bd"]  ;   
    ["bar_equal_greater.liga" "|=>" "\xe1be"]  ;   
    ["bar_greater.liga" "|>" "\xe1bf"]  ;   
    ["bar_bracketright.liga" "|]" "\xe1c0"]  ;   
    ["bar_bar.liga" "||" "\xe1c1"]  ;   
    ["bar_bar_hyphen.liga" "||-" "\xe1c2"]  ;   
    ["bar_bar_equal.liga" "||=" "\xe1c3"]  ;   
    ["bar_bar_greater.liga" "||>" "\xe1c4"]  ;   
    ["bar_bar_bar_greater.liga" "|||>" "\xe1c5"]  ;   
    ["bar_braceright.liga" "|}" "\xe1c6"]  ;   
    ["braceright.case" "}" "\xe1c7"]  ;   
    ["asciitilde_hyphen.liga" "~-" "\xe1c8"]  ;   
    ["asciitilde_equal.liga" "~=" "\xe1c9"]  ;   
    ["asciitilde_greater.liga" "~>" "\xe1ca"]  ;   
    ["asciitilde_at.liga" "~@" "\xe1cb"]  ;   
    ["asciitilde_asciitilde.liga" "~~" "\xe1cc"]  ;   
    ["asciitilde_asciitilde_greater.liga" "~~>" "\xe1cd"]  ;   
    ["exclamdown.case" "¡" "\xe1ce"]  ;   
    ["dieresis.case" "¨" "\xe1cf"]  ;   
    ["guillemotleft.case" "«" "\xe1d0"]  ;   
    ["uni00AD.case" "­" "\xe1d1"]  ;   
    ["macron.case" "¯" "\xe1d2"]  ;   
    ["acute.case" "´" "\xe1d3"]  ;   
    ["acute.case.loclPLK" "´" "\xe1d4"]  ;   
    ["acute.loclPLK" "´" "\xe1d5"]  ;   
    ["cedilla.case" "¸" "\xe1d6"]  ;   
    ["guillemotright.case" "»" "\xe1d7"]  ;   
    ["questiondown.case" "¿" "\xe1d8"]  ;   
    ["Oacute.loclPLK" "Ó" "\xe1d9"]  ;   
    ["oacute.loclPLK" "ó" "\xe1da"]  ;   
    ["Cacute.loclPLK" "Ć" "\xe1db"]  ;   
    ["cacute.loclPLK" "ć" "\xe1dc"]  ;   
    ["Nacute.loclPLK" "Ń" "\xe1dd"]  ;   
    ["nacute.loclPLK" "ń" "\xe1de"]  ;   
    ["Sacute.loclPLK" "Ś" "\xe1df"]  ;   
    ["sacute.loclPLK" "ś" "\xe1e0"]  ;   
    ["Zacute.loclPLK" "Ź" "\xe1e1"]  ;   
    ["zacute.loclPLK" "ź" "\xe1e2"]  ;   
    ["circumflex.case" "ˆ" "\xe1e3"]  ;   
    ["caron.alt" "ˇ" "\xe1e4"]  ;   
    ["caron.case" "ˇ" "\xe1e5"]  ;   
    ["breve.case" "˘" "\xe1e6"]  ;   
    ["dotaccent.case" "˙" "\xe1e7"]  ;   
    ["ring.case" "˚" "\xe1e8"]  ;   
    ["tilde.case" "˜" "\xe1e9"]  ;   
    ["hungarumlaut.case" "˝" "\xe1ea"]  ;   
    ["uni0326.case" "̦" "\xe1eb"]  ;   
    ["tonos.case" "΄" "\xe1ec"]  ;   
    ["uni1FEF.case" "`" "\xe1ed"]  ;   
    ["uni1FFD.case" "´" "\xe1ee"]  ;   
    ["uni2007.tf" " " "\xe1ef"]  ;   
    ["endash.case" "–" "\xe1f0"]  ;   
    ["emdash.case" "—" "\xe1f1"]  ;   
    ["guilsinglleft.case" "‹" "\xe1f2"]  ;   
    ["guilsinglright.case" "›" "\xe1f3"]  ;   
    ["minus.dnom" "−" "\xe1f4"]  ;   
    ["minus.numr" "−" "\xe1f5"]  ;   
    ["infinity.case" "∞" "\xe1f6"]  ;   
))
