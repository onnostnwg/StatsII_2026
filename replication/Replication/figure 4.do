clear all


cd "YOURPATH\Replication"


set scheme plottigblind



use "data\data_wahlwechsel.dta"

recode Wahlwechsel (1 = 0) (2 = 1), gen(ww)

 * 0 -1 kodiert
 set scheme white_tableau
 
logit ww OpennessMittelwert ConscientiousnessMittelwert ExtraversionMittelwert AgreeablenessMittelwert NeuroticismMittelwert LiRe alter i.geschlecht i.bildung i.haushaltseinkommen i.region [pweight = ipf_gewicht], or
 margins, at(OpennessMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot1.gph", replace) xtitle("Openness") ytitle("Predicted probability for vote switching") title("")

 margins, at(ConscientiousnessMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot2.gph", replace) xtitle("Conscientiousness") ytitle("") title("") 
 
  margins, at(ExtraversionMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot3.gph", replace) xtitle("Extraversion") ytitle("") title("")
 
   margins, at(AgreeablenessMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot4.gph", replace) xtitle("Agreeableness")  ytitle("") title("")

    margins, at(NeuroticismMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot5.gph", replace) xtitle("Neuroticism")  ytitle("") title("")
 
 graph combine "dump/plot1.gph" "dump/plot2.gph" "dump/plot3.gph" "dump/plot4.gph" "dump/plot5.gph", ycom row(1) xsize(6) ysize(1.5) scale(1) title("") subtitle("Vote switching") imargin(0 0 0 0)
 graph save "dump/predprob_voteswitching.gph", replace
 
clear all 
use "data/data_populismus.dta"
 
gen pop = .
replace pop = 0 if Populismusanfällige == 1 
replace pop = 1 if Populismusanfällige == 2 
 
logit pop OpennessMittelwert ConscientiousnessMittelwert ExtraversionMittelwert AgreeablenessMittelwert NeuroticismMittelwert LiRe alter i.geschlecht i.bildung i.haushaltseinkommen i.region [pweight = ipf_gewicht] , or


 margins, at(OpennessMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot1.gph", replace) xtitle("Openness") ytitle("Predicted probability for vote switching to AfD") title("")

 margins, at(ConscientiousnessMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot2.gph", replace) xtitle("Conscientiousness") ytitle("") title("") 
 
  margins, at(ExtraversionMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot3.gph", replace) xtitle("Extraversion") ytitle("") title("")
 
   margins, at(AgreeablenessMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot4.gph", replace) xtitle("Agreeableness")  ytitle("") title("")

    margins, at(NeuroticismMittelwert=(0(0.1)1)) atmeans
 marginsplot, saving("dump/plot5.gph", replace) xtitle("Neuroticism")  ytitle("") title("")
 
  graph combine "dump/plot1.gph" "dump/plot2.gph" "dump/plot3.gph" "dump/plot4.gph" "dump/plot5.gph", ycom row(1) xsize(6) ysize(1.5) scale(1) title("") subtitle("Vote switching to AfD = susceptibility to populism") imargin(0 0 0 0)
 graph save "dump/predprob_pop.gph", replace 
 
 graph combine "dump/predprob_voteswitching.gph" "dump/predprob_pop.gph", row(2) imargin(0 0 0 0 )

 * graph edits
			gr_edit .plotregion1.graph1.subtitle.style.editstyle size(small) editcopy
			// subtitle size

			 gr_edit .plotregion1.graph2.subtitle.style.editstyle size(small) editcopy
			// subtitle size

			 gr_edit .plotregion1.graph1.plotregion1.graph1.yaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph2.plotregion1.graph1.yaxis1.title.style.editstyle margin(medsmall) editcopy
			// title margin

			 gr_edit .plotregion1.graph1.plotregion1.graph1.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph1.plotregion1.graph2.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph1.plotregion1.graph3.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph1.plotregion1.graph4.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph1.plotregion1.graph5.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph2.plotregion1.graph1.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph2.plotregion1.graph2.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph2.plotregion1.graph3.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph2.plotregion1.graph4.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph2.plotregion1.graph5.xaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			 gr_edit .plotregion1.graph2.plotregion1.graph1.yaxis1.title.style.editstyle size(medsmall) editcopy
			// title size

			gr_edit .AddTextBox added_text editor 95.25817018275305 -1.246227871876972
			gr_edit .added_text_new = 1
			gr_edit .added_text_rec = 1
			gr_edit .added_text[1].style.editstyle  angle(default) size(huge) color(black) horizontal(left) vertical(middle) margin(zero) linegap(zero) drawbox(no) boxmargin(zero) fillcolor(bluishgray) linestyle( width(vthin) color(black) pattern(solid) align(inside)) box_alignment(east) editcopy
			gr_edit .added_text[1].style.editstyle size(medium) editcopy
			gr_edit .added_text[1].text = {}
			gr_edit .added_text[1].text.Arrpush a
			// editor text[1] edits

			gr_edit .AddTextBox added_text editor 46.35567197717231 -.8994016434685981
			gr_edit .added_text_new = 2
			gr_edit .added_text_rec = 2
			gr_edit .added_text[2].style.editstyle  angle(default) size(huge) color(black) horizontal(left) vertical(middle) margin(zero) linegap(zero) drawbox(no) boxmargin(zero) fillcolor(bluishgray) linestyle( width(vthin) color(black) pattern(solid) align(inside)) box_alignment(east) editcopy
			gr_edit .added_text[2].style.editstyle size(medium) editcopy
			gr_edit .added_text[2].text = {}
			gr_edit .added_text[2].text.Arrpush b
			// editor text[2] edits

 graph export "figures\Figure 4.pdf", replace
 graph export "figures\Figure 4.emf", replace 
 
 
 