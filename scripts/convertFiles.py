import os
from fpdf import FPDF

#loops through the data directory, finds all .txt files and converts them to pdfs
#requires the python package fpdf
#to install: pip install fpdf
#to run: python convertFiles.py
# Define a function to convert a text file to PDF
def convert_to_pdf(file_path):
  pdf = FPDF()
  pdf.add_page()
  with open(file_path, 'r') as file:
    text = file.read()
    pdf.set_font("Arial", size=12)
    pdf.multi_cell(0, 10, text)
  pdf_path = os.path.splitext(file_path)[0] + '.pdf'
  pdf.output(pdf_path)

# Define the directory to search for text files
data_dir = '../data'

# Loop through the data directory and convert each text file to PDF
for root, dirs, files in os.walk(data_dir):
  for file in files:
    if file.endswith('.txt'):
      file_path = os.path.join(root, file)
      convert_to_pdf(file_path)


