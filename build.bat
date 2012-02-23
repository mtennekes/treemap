@echo off


echo Removing building information...
rm -rf output

echo Build documentation...
R -q -f build/roxygen.R

md output
cd output
R CMD build ../pkg
FOR %%1 in (*.tar.gz) DO R CMD check %%1
