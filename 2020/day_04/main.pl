use Time::HiRes qw(time);

time_func(\&part_one);
time_func(\&part_two);

sub time_func {
    my $f = shift;
    my $start = time();
    my $ans = $f->();
    my $elapsed = time() - $start;
    printf("(%dms)\t%d\n", $elapsed * 1000, $ans);
}

sub part_two {
    open(my $fh, "<", "input")
      or die "Can't open < input.txt: $!";

    my %hash    = ();
    my $counter = 0;
    while (<$fh>) {
        if ($_ eq "\n") {
            check2(\%hash, \$counter);
            %hash = ();
        } else {
            add_line(\%hash, $_);
        }
    }
    check2(\%hash, \$counter);    # Don't forget the last passport!
    return $counter;
}

sub check2 {
    my $hash_ref    = shift;
    my $counter_ref = shift;

    my @fs = ("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid");
    foreach (@fs) {
        return if !(exists $hash_ref->{$_});
    }

    return if bounds($hash_ref, "byr", 1920, 2002);
    return if bounds($hash_ref, "iyr", 2010, 2020);
    return if bounds($hash_ref, "eyr", 2020, 2030);

    # Height
    return if !(exists $hash_ref->{"hgt"});
    my $hgt = $hash_ref->{"hgt"};
    my $type = substr($hgt, -2);

    my $str = substr($hgt, 0, -2);
    return if (length($str) eq 0);
    return if (($type eq "cm") and ($str lt 150 or $str gt 193));
    return if (($type eq "in") and ($str lt 59 or $str gt 76));

    return if !(exists $hash_ref->{"hcl"});
    my $hcl = $hash_ref->{"hcl"};
    return if !($hcl =~ /#[a-f0-9]{6}/);

    # eye color
    return if !(exists $hash_ref->{"ecl"});
    my $ecl = $hash_ref->{"ecl"};
    my @array = ("amb", "blu", "brn", "gry", "grn", "hzl", "oth");

    my %params = map { $_ => 1 } @array;
    return if !(exists($params{$ecl}));

    # pid
    return if !(exists $hash_ref->{"pid"});
    my $pid = $hash_ref->{"pid"};
    return if !($pid =~ /^^[0-9]{9}$/);

    ++$$counter_ref;
}

sub bounds {
    my $hash_ref = shift;
    my $str      = shift;
    my $n1       = shift;
    my $n2       = shift;

    if (!(exists $hash_ref->{$str})) {
        return 1;
    }
    my $val = $hash_ref->{$str};
    return ($val < $n1 or $val > $n2);
}

sub part_one {
    open(my $fh, "<", "input")
      or die "Can't open < input.txt: $!";

    my %hash    = ();
    my $counter = 0;
    while (<$fh>) {
        if ($_ eq "\n") {
            check(\%hash, \$counter);
            %hash = ();
        } else {
            add_line(\%hash, $_);
        }
    }
    check(\%hash, \$counter);    # Don't forget the last passport!
    return $counter;
}

sub add_line {
    my ($hashRef, $line) = @_;

    foreach my $i (split(" ", $line)) {
        my @spl = split(":", $i);
        my $key = @spl[0];
        my $val = @spl[1];

        $hashRef->{$key} = $val;
    }
}

sub check {
    my $hash_ref    = shift;
    my $counter_ref = shift;

    my $k = keys %{$hash_ref};

    if (($k eq 8) or (($k eq 7) and !(exists $hash_ref->{'cid'}))) {
        ++$$counter_ref;
    }
}
