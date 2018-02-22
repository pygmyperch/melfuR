use strict; use warnings;
#usage perl gINLAnd_input.pl file.gen
my $Line_1 = 1;
#the genotypes are coded 001, 002, 003, and 004 (A, T, C, and G) and each individual has two values 
my @Valid_Genotypes = ("010", "011", "012", "013", "010", "011", "012", "013");# you can modify this line to suit your genepop file
my $NO_DATA = "000";

sub Is_Valid_Genotype {
   my $n = scalar(@_);
   my $result = "FALSE";
   if ($n != 1) {
     print "too many paramters passed in\n"; 
   } else {
     foreach my $item (@_){
       foreach my $value (@Valid_Genotypes) {
         if ($item eq $value) {  
           $result = "TRUE"; } } } }
  return $result;
}

my @files = grep ( -f ,<*.gen>);

my $num_args = $#files + 1;
if ($num_args <= 0 ) { die "no arguments found, you must provide at least on input file name";}

foreach my $infile (@files) {
  # Create and open the required files
  #
  my $filename=$infile;
  $filename=~s/.gen//; #Stripped file name to create a matched out file name

  my $IN_DATA_FILE_NAME = "$infile";
  my $OUTFILE_NAME = "$filename-Allele_Counts";
  my $POPULATION_OUTFILE_NAME = "$filename-Population_Counts";
  open(IN_DATA_FILE, $IN_DATA_FILE_NAME) || die "can't open in file $IN_DATA_FILE_NAME, aborting attempt";
  open (OUTFILE, '>', $OUTFILE_NAME) || die "Can't open the output file. exiting"; 
  open (OUTFILE2, '>', $POPULATION_OUTFILE_NAME) || die "Can't open the output file. exiting"; 
  # Variables in scope per input file.
  my $Individual_Count = 0;
  my $line_Count = 0;
  my $MARKER_DONE = "FALSE";
  my $MARKER_COUNT = 0;
  my $Population_Count = 0;  
  my @Markers = ();
  my @Population_Marker_Count = ();
  my @Population_Allele_Count = ();
  my $NO_DATA = "000";
  my @Population_Key;
  my @Individual_Marker;
  my $OK = "FALSE";


  # Process the data from the current input file
  #
  while (<IN_DATA_FILE>) {
   chomp;
   $line_Count++;
   # Find the list of Markers for this file, ignoring the files first line. 
   if ($line_Count != $Line_1) {
     if ("$MARKER_DONE" eq "FALSE") {   
      if ("$_" ne "Pop") {
          $MARKER_COUNT++;
          my $MARKER_Index = $MARKER_COUNT - 1;
          $Markers[$MARKER_Index] = $_;
      } else {
          # we've got the marker list
          $MARKER_DONE = "TRUE";
          # Create the first populations key_set
          @Population_Key = ($NO_DATA) x $MARKER_COUNT;
          @Individual_Marker = ($NO_DATA) x $MARKER_COUNT;
          push @Population_Marker_Count, [ (0) x $MARKER_COUNT ] for (0);
          push @Population_Allele_Count, [ (0) x $MARKER_COUNT ] for (0); }
   } else {
   # At this point we've pulled off the marker info and need to start to construct the population data.
     if ("$_" eq "Pop") {
        $Population_Count++;
        $Individual_Count = 0;
        push @Population_Marker_Count, [ (0) x $MARKER_COUNT ] for ($Population_Count);
        push @Population_Allele_Count, [ (0) x $MARKER_COUNT ] for ($Population_Count);
      } else {
         $OK = "FALSE";
         my @Individual_Alleles;
         # take the current line and break it up into tokens.
         my @tokens = split(' ', $_);
         #ignore the first 2 tokens (the individual identifier and the comma.)          
          for (my $i = 2; $i <@tokens; $i++) {
            # If that's the case we need to dump the data as we don't know how it fits.
            $Individual_Alleles[$i - 2] = $tokens[$i];
            if (($i - 1) == $MARKER_COUNT) {$OK = "TRUE"; } else {$OK = "FALSE";}
          }
          # check that this has the correct number of datapoints - ignore it if not.
          if ($OK eq "TRUE") {
              # If this is the first individual of the first population in the file, it contains the keys, so save them.
              # SABR TBD: need to handle the case where the first individual in the population had bad data, but the rest don't
              # so also check if the stored key if invalid and try and find one if it's not. 
              if (($Individual_Count == 0) && ($Population_Count == 0)) {
                 # find the first genotype and then start the count.
                 # loop through the individuals alleles
                 # split them into single genotypes and count for the first one                 
                 for (my $m = 0; $m <@Markers; $m++) {
                      my @Genome = (substr( $Individual_Alleles[$m], 0, 3 ), substr( $Individual_Alleles[$m], 3, 3 ));
                      $Population_Key[$m] = $Genome[0];
                 }
              } 
              #process each individual (including the one the key came from) to count the matches
              for (my $m = 0; $m <@Markers; $m++) {
                 #check if we have a valid population key, if not see if this individual can provide one before progressing with the processing
                 if (Is_Valid_Genotype($Population_Key[$m]) ne "TRUE") {
                    my @Genome = (substr( $Individual_Alleles[$m], 0, 3 ), substr( $Individual_Alleles[$m], 3, 3 ));
                      $Population_Key[$m] = $Genome[0];
                 }
                 #Now continue on with the count.
                 if (Is_Valid_Genotype($Population_Key[$m]) eq "TRUE") {
                   $Individual_Marker[$m] = $Individual_Alleles[$m];
                  
                   my @value = (substr( $Individual_Alleles[$m], 0, 3 ), substr( $Individual_Alleles[$m], 3, 3 ));
                   for (my $v = 0; $v <@value; $v++) {
                     if ($value[$v] eq $Population_Key[$m]) {
                        my $temp_marker_count = $Population_Marker_Count[$Population_Count][$m];
                        $Population_Marker_Count[$Population_Count][$m] = $temp_marker_count + 1;
                     }
                     if ($value[$v] ne $NO_DATA) {
                        my $temp_allele_count = $Population_Allele_Count[$Population_Count][$m];
                        $Population_Allele_Count[$Population_Count][$m] = $temp_allele_count + 1;
                     }
                   }
                 }
               }
              # at this point, the individuals counts have been added to the populations counts              
             $Individual_Count++;
            } else {
               #skip this one, but record the event.
               print "Dropped a line on input during parse of population $Population_Count, Individual $Individual_Count due to incorrect number of data points\n";
            }
     }
   } # end of the poplution search
 }#exclude the first line
} # end of file read

  # END THE DATA FILE PROCESSING
  
  close(IN_DATA_FILE);
  #write to the outputfile
  # MARKERS
  foreach (my $i = 0; $i <@Markers; $i++) {
  #print "$Markers[$i]";
    if ($i != $#Markers) {
      print OUTFILE "$Markers[$i] ";
      print OUTFILE2 "$Markers[$i] ";
    } else {
      print OUTFILE "$Markers[$i]\n";
      print OUTFILE2 "$Markers[$i]\n";
    }
  }
  # COUNTS
  foreach (my $population = 0; $population < $Population_Count +1; $population++) {
    for (my $count = 0; $count <$MARKER_COUNT; $count++) {
        if ($count != $#Markers) { 
          print OUTFILE "$Population_Marker_Count[$population][$count] ";
          print OUTFILE2 "$Population_Allele_Count[$population][$count] ";
        } else {
          print OUTFILE "$Population_Marker_Count[$population][$count]\n";
          print OUTFILE2 "$Population_Allele_Count[$population][$count]\n";
        }
    }
  }
  close (OUTFILE);
  close (OUTFILE2);
}



