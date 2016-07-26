#!/usr/bin/perl
use strict;
use warnings;

-d $ARGV[0] or die "Missing name of directory to collect!";

sub read_csv($) {
  chomp(my $line = $_[0]);
  $line =~ /^"?([^,"]*)"?,([^,]*)(,|$)/ or die "Bad line $line!";
  return ($1, $2);
}

my %tests;
my %variants;

# Read individual results
foreach my $csv (glob "$ARGV[0]/*[!m].csv") {
  # Read time
  $csv =~ m#/([^/]*)_[0-9]*m?.csv$# or die "Bad csv filename $csv!";
  my $variant = $1;
  $variants{$variant} = 1;
  open(my $f,"<", $csv) or die "Cannot open file $csv!";
  my $_header = <$f>;
  foreach my $line (<$f>) {
    my ($name, $res) = read_csv($line);
    $tests{$name} = {} unless exists $tests{$name};
    exists $tests{$name}->{$variant} and die "Repeated data $name-$variant!";
    $tests{$name}->{$variant} = [$res];
  }
  close($f);

  # Read memory
  $csv =~ s#\.csv$#m.csv# or die "Bad csv filenaem $csv!";
  open($f,"<", $csv) or die "Cannot open file $csv!";
  $_header = <$f>;
  foreach my $line (<$f>) {
    my ($name, $res) = read_csv($line);
    length @{$tests{$name}->{$variant}} == 1 or die "Repeated data $name-$variant!";
    push @{$tests{$name}->{$variant}}, $res;
  }
  close($f);
}

# Write merged csv
open (my $f, ">", "$ARGV[0].csv") or die "Cannot open $ARGV[0].csv!";
print $f ";";
foreach my $v (sort keys %variants) {
  print $f "$v;;";
}
print $f "\n";

foreach my $test (sort keys %tests) {
  print $f "$test;";
  foreach my $v (sort keys %variants) {
    if (exists $tests{$test}->{$v}) {
      print $f "$tests{$test}->{$v}->[0];$tests{$test}->{$v}->[1];";
    } else {
      print $f ";;";
    }
  }
  print $f "\n";
}

# Write sizes of benchmark and modules.
print $f "\nFile;Size\n";
foreach my $n ("../dist/build/benchmark/benchmark",
               "../dist/build/benchmark/benchmark-tmp/Container/Map.o",
               "../dist/build/benchmark/benchmark-tmp/Container/Set.o",
               "../dist/build/benchmark/benchmark-tmp/Container/IntMap.o",
               "../dist/build/benchmark/benchmark-tmp/Container/IntSet.o") {
  $n =~ m#/([^/]*)$#;
  print $f "$1;" . (-s $n) . "\n";
}

close($f);
