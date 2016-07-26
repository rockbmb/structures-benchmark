#!/usr/bin/perl
use strict;
use warnings;

sub usage() {
  print "Usage: $0 original.csv new.csv";
  exit 1;
}

-f "$ARGV[0]" or usage();
-f "$ARGV[1]" or usage();

open (my $fori, "<", $ARGV[0]) or die "Cannot open $ARGV[0]!";
open (my $fnew, "<", $ARGV[1]) or die "Cannot open $ARGV[1]!";

my @totals;
sub print_totals() {
  for my $mean ("geo", "ari") {
    print "total ($mean mean)";
    for my $i (1..$#totals) {
      print ";";
      next unless @{$totals[$i]};
      my $avg;
      if ($mean eq "geo" ) {
        $avg = 1.0;
        foreach my $v (@{$totals[$i]}) { $avg *= $v; }
        $avg **= 1/@{$totals[$i]};
      } elsif ($mean eq "ari") {
        $avg = 0.0;
        foreach my $v (@{$totals[$i]}) { $avg += $v; }
        $avg /= @{$totals[$i]};
      }
      printf "%.1f%%", 100 * $avg - 100;
    }
    print "\n";
  }
  @totals = ();
}

my ($lori, $lnew);
while ($lori = <$fori> and $lnew = <$fnew>) {
  chomp($lori);
  chomp($lnew);

  my @cori = split /;/, $lori;
  my @cnew = split /;/, $lnew;

  die "Nonequal number of columns on line $.!" if scalar(@cori) != scalar(@cnew);

  for my $i (0..$#cori) {
    my ($ori, $new) = ($cori[$i], $cnew[$i]);
    die "Empty versus nonempty column $i on line $.!" if length($ori) > 0 && length($new) == 0;
    die "Empty versus nonempty column $i on line $.!" if length($ori) == 0 && length($new) > 0;

    print ";" if $i > 0;
    if ($ori =~ /^[0-9.e+-]+$/ && $new =~ /^[0-9.e+-]+$/) {
      printf "%.1f%%", 100 * $new / $ori - 100;
      $totals[$i] = [] unless defined $totals[$i];
      push @{$totals[$i]}, $new / $ori;
    } elsif ($ori ne $new) {
      die "Different nonnumeric column $i on line $., '$ori' vs '$new'!";
    } else {
      print $ori;
    }
  }
  print_totals unless @cori;
  print "\n";
}
print_totals if @totals;
