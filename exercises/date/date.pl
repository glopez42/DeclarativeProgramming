weekday('Monday').
weekday('Tuesday').
weekday('Wednesday').
weekday('Thursday').
weekday('Friday').
weekday('Saturday').
weekday('Sunday').

numberday31(1).
numberday31(2).
numberday31(3).
numberday31(4).
numberday31(5).
numberday31(6).
numberday31(7).
numberday31(8).
numberday31(9).
numberday31(10).
numberday31(11).
numberday31(12).
numberday31(13).
numberday31(14).
numberday31(15).
numberday31(16).
numberday31(17).
numberday31(18).
numberday31(19).
numberday31(20).
numberday31(21).
numberday31(22).
numberday31(23).
numberday31(24).
numberday31(25).
numberday31(26).
numberday31(27).
numberday31(28).
numberday31(29).
numberday31(30).
numberday31(31).

numberday30(1).
numberday30(2).
numberday30(3).
numberday30(4).
numberday30(5).
numberday30(6).
numberday30(7).
numberday30(8).
numberday30(9).
numberday30(10).
numberday30(11).
numberday30(12).
numberday30(13).
numberday30(14).
numberday30(15).
numberday30(16).
numberday30(17).
numberday30(18).
numberday30(19).
numberday30(20).
numberday30(21).
numberday30(22).
numberday30(23).
numberday30(24).
numberday30(25).
numberday30(26).
numberday30(27).
numberday30(28).
numberday30(29).
numberday30(30).

numberdayFeb(1).
numberdayFeb(2).
numberdayFeb(3).
numberdayFeb(4).
numberdayFeb(5).
numberdayFeb(6).
numberdayFeb(7).
numberdayFeb(8).
numberdayFeb(9).
numberdayFeb(10).
numberdayFeb(11).
numberdayFeb(12).
numberdayFeb(13).
numberdayFeb(14).
numberdayFeb(15).
numberdayFeb(16).
numberdayFeb(17).
numberdayFeb(18).
numberdayFeb(19).
numberdayFeb(20).
numberdayFeb(21).
numberdayFeb(22).
numberdayFeb(23).
numberdayFeb(24).
numberdayFeb(25).
numberdayFeb(26).
numberdayFeb(27).
numberdayFeb(28).


monthday(Day, Month):-
    Month = 'January',
    numberday31(Day).

monthday(Day, Month):-
    Month = 'February',
    numberdayFeb(Day).

monthday(Day, Month):-
    Month = 'March',
    numberday31(Day).

monthday(Day, Month):-
    Month = 'April',
    numberday30(Day).

monthday(Day, Month):-
    Month = 'May',
    numberday31(Day).

monthday(Day, Month):-
    Month = 'June',
    numberday30(Day).

monthday(Day, Month):-
    Month = 'July',
    numberday31(Day).

monthday(Day, Month):-
    Month = 'August',
    numberday31(Day).

monthday(Day, Month):-
    Month = 'September',
    numberday30(Day).

monthday(Day, Month):-
    Month = 'October',
    numberday31(Day).

monthday(Day, Month):-
    Month = 'November',
    numberday30(Day).

monthday(Day, Month):-
    Month = 'December',
    numberday31(Day).

date(Weekday, Day, Month):-
    weekday(Weekday),
    monthday(Day,Month).


