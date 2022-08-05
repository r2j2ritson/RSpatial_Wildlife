# Combine Cover letter, Resume, and Reference PDFs into one File

make_pdf_app <- function(cl,cv,ref,app_name){
  # Load packages
  lapply(c('pdftools','doconv'), function(pkg){
    if(!nzchar(system.file(package=pkg))){
      install.packages(pkg)
    }
    require(pkg, character.only = T)
  })
  
  # Check file names are PDFs, convert if Word Document
  if(grepl(".docx",cl)){
    message("Cover letter is a Word document: Converting to PDF")
    doconv::docx2pdf_install(force = T)
    cl<- doconv::docx2pdf(cl)
    doconv::docx2pdf_uninstall()
    }
  if(grepl(".docx",cv)){
    message("Resume/CV is a Word document: Converting to PDF")
    doconv::docx2pdf_install(force = T)
    cv<- doconv::docx2pdf(cv)
    doconv::docx2pdf_uninstall()
    }
  if(grepl(".docx",ref)){
    message("References is a Word document: Converting to PDF")
    doconv::docx2pdf_install(force = T)
    ref<- doconv::docx2pdf(ref)
    doconv::docx2pdf_uninstall()
    }
  if(grepl(".docx",app_name)){
    message("Final Application is a Word document: Converting to PDF")
    app_name <- gsub(".docx",".pdf",app_name)
    }

  # Combine files into one document
  pdftools::pdf_combine(c(cl,cv,ref), app_name)
}

# NOT RUN
#setwd("C:/Users/Documents")
#make_pdf_app(cl="CoverLetter.docx",
#            cv="Resume.docx",
#             ref="References.docx",
#             app_name = "Application.pdf")
# END NOT RUN
