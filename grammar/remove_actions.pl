#!/usr/bin/perl
use strict;
use warnings;

# Read the entire input into a single string
local $/ = undef;
my $input = <STDIN>;

# Function to check if the block should be kept
sub should_keep {
    my $block = shift;
    # Keep the block if it contains 'panic'
    return $block =~ /panic/;
}

# Process input to handle semantic actions
my @parts;
my $depth = 0;
my $buffer = "";
my $current_block = "";
my $in_string = 0;

foreach my $char (split //, $input) {
    if ($char eq '"' and $buffer !~ /\\$/) {
        $in_string = !$in_string;
    }
    if (!$in_string) {
        if ($char eq '{') {
            $depth++;
            if ($depth == 1) {
                push @parts, $buffer;
                $buffer = "";
                $current_block = "{";
                next;
            }
        } elsif ($char eq '}') {
            $depth--;
            if ($depth == 0) {
                $current_block .= "}";
                push @parts, (should_keep($current_block) ? $current_block : "");
                $current_block = "";
                next;
            }
        }
    }
    if ($depth > 0) {
        $current_block .= $char;
    } else {
        $buffer .= $char;
    }
}

push @parts, $buffer if $buffer;

# Remove variable names
my $output = join('', @parts);
$output =~ s/\b\w+://g;

# Removes trailing spaces on each line
$output =~ s/ +$//mg;

# Replaces multiple space characters with a single space, except at the start of a line
$output =~ s/(?<=\S) {2,}/ /g;

# Removes space before a ")" character
$output =~ s/ (?=\))/)/g;

# Print the modified input
print $output;
