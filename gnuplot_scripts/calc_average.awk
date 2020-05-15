# This awk script takes a data file and adds a final row to it
# that takes the average of the normalized values of the rows above it

BEGIN{n=-1;}
{
    ## echo the original row
    print $0;

    ## Skip the first row with headings
    if (n != -1) {
        ## Add normalized sum
        for (i = 2; i <= NF; i++)
            sums[i-2] += $i/$2;
    }
    n++;
}
END{
    printf "average";
    for (i = 0; i in sums; i++)
        printf " %f", sums[i]/n;
    printf "\n";
}
