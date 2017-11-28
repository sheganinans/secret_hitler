while :
do
    inotifywait -qq main.exe
    killall -q main.exe
    #psql -f dropall.sql test
    #psql -f step99.sql test
    sleep 1
    ./main.exe -q &
    touch watchfile
done

