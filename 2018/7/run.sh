exec 3<>a
exec 4<>b
exec 5<>c
exec 6<>d
exec 7<>e

run_amplifiers() {
    echo running $1 $2 $3 $4 $5

    python x.py input.txt <&3 >&4 &
    python x.py input.txt <&4 >&5 &
    python x.py input.txt <&5 >&6 &
    python x.py input.txt <&6 >&7 &
    python x.py input.txt <&7 >> output.txt &

    echo $1 >&3
    echo $2 >&4
    echo $3 >&5
    echo $4 >&6
    echo $5 >&7

    echo 0 >&3

}


cat /dev/null > output.txt
cat permutations.txt | while read ln; do
    sleep 0.05
    run_amplifiers $ln
done

exec 3>&-
exec 4>&-
exec 5>&-
exec 6>&-
exec 7>&-
