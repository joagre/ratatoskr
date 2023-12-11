#!/usr/bin/perl
use strict;
use warnings;

# Read the entire input into a single string
local $/ = undef;
my $input = <STDIN>;

# Remove semantic actions ({} blocks), handling multiline
$input =~ s/\{.*?\}//gs;

# Remove variable names (e.g., 'i:' in 'i:Import')
$input =~ s/\b\w+://g;

# Print the modified input
print $input;
