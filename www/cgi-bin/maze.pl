#!/usr/bin/perl 
use strict;
print "Content-type: text/HTML\n\n";

my $width = 20;
my $height = 10;
my $url = "[<a href=\"http://fogus.me\" style=\"text-decoration:none;\" target=\"_top\"> <- </a> | <a href=\"http://www.earthvssoup.com\" style=\"text-decoration:none;\" target=\"_top\"> -> </a>]";
my @full = map { [ (1) x $width ] } (1..$height);
my @closedleft = map { [ (1) x $width ] } (1..$height);
my @closedbottom = map { [ (1) x $width ] } (1..$height);

my @stack = ([0, 0]);

sub print_header {
    print "<html>\n<head>\n\t<title>Minotaur Computing</title>\n";
    print "<link rel=StyleSheet HREF=\"../styles/ui.css\" TYPE=\"text/css\" MEDIA=screen></head>\n";
    print "<body text=\"#c9c9ff\"><pre><center>\n";
}

sub print_footer {
    print "</center></pre></body></html>\n";
}

sub in_bounds {
	my ($row, $col) = @_;
	return 0 if $row < 0 or $col < 0 or $col >= $width or $row >= $height;
	return 1;
}

sub go_direction {
	my ($row, $col, $dir) = @_;
	if ($dir eq 'l') {
		return ($row, $col - 1);
	} elsif ($dir eq 'r') {
		return ($row, $col + 1);
	} elsif ($dir eq 'u') {
		return ($row - 1, $col);  
	} elsif ($dir eq 'd') {
		return ($row + 1, $col);
	} else {
		die "wrong direction $dir";
	}
}

sub pick_direction {
	my ($row, $col) = @_;
	my @directions = ();
	my $direction;
	# my $fulness = join '', map { join '', @{$_}, "\n" } @full;
	# print STDERR $fulness;
	for $direction (qw(l r u d)) {
		my @possible = go_direction($row, $col, $direction);
		push @directions, $direction if in_bounds (@possible) 
			and $full[$possible[0]]->[$possible[1]];
		# warn "fulness: $direction is $full[$possible[0]]->[$possible[1]]\n";
	}
	# warn "possibilities: " . join (', ', @directions) . "\n";
	return undef if not @directions;
	my $rv = rand @directions;
	# warn "dir $directions[$rv]\n";
	return $directions[$rv];
}

sub open_cell {
	$full[$_[0]]->[$_[1]] = 0;
}

sub open_wall {
	my ($row, $col, $dir) = @_;
	if ($dir eq 'l') {
		$closedleft[$row]->[$col] = 0;
	} elsif ($dir eq 'r') {
		$closedleft[$row]->[$col+1] = 0;
	} elsif ($dir eq 'u') {
		$closedbottom[$row-1]->[$col] = 0;
	} elsif ($dir eq 'd') {
		$closedbottom[$row]->[$col] = 0;
	} else {
		die "wrong direction in open_wall $dir\n";
	}
}

print_header();

my $direction;
my @location;
while (@stack) {
	@location = @{$stack[$#stack]};
	# warn "At $location[0], $location[1]\n";
	open_cell(@location);
	$direction = pick_direction(@location);
	if (not defined $direction) {
		pop @stack;
		next;
	}
	open_wall(@location, $direction);
	push @stack, [go_direction(@location, $direction)];
}

# print the maze

my ($i, $j);
print "1  ", "000" x ($width-2), "0  1\n";
for $i (0..$height-1) {
	for $j (0..$width-1) {
		print $closedleft[$i]->[$j] ? "1  " : "   ";
	}
	print "1\n";
	for $j (0..$width-1) {
		print $closedbottom[$i]->[$j] ? "100" : "1  ";
	}
	print "1\n";
}

print $url;
print_footer();
