all: PA1_template.html

PA1_template.html: PA1_template.Rmd
	R -e "rmarkdown::render('PA1_template.Rmd')"
	open PA1_template.html 

.PHONY: clean
clean: 
	rm -rf PA1_template.html PA1_template.md PA1_template_files activity.csv
