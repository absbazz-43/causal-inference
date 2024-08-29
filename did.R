tfile_path <- tempfile()
tdir_path <- tempdir()

# Download zip file
download.file("http://davidcard.berkeley.edu/data_sets/njmin.zip", 
              destfile = tfile_path)

download.file("http://davidcard.berkeley.edu/data_sets/njmin.zip", 
              destfile = tfile_path)
unzip(tfile_path, exdir = tdir_path)