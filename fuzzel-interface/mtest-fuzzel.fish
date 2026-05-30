#!/usr/bin/env fish

function main_menu
  switch $argv	
    case '1. Profile Setup'
      set -l qfile (ls *.json | fuzzel -d -p 'Questions Database:')
      if test -n "$qfile"
        mtest profile $qfile
      end
      run_gui
    case '2. Create Exam'
      set -l qfile (ls *.json | fuzzel -d -p 'Questions Database:')
      if test -n "$qfile"
        mtest create $qfile
      end
      run_gui
    case '3. Mark Papers'
      set -l afile (ls *.csv | fuzzel -d -p 'Given Answers:')
      if test -n "$afile"
        mtest mark $afile
      end
      run_gui
    case '4. Backup Exam'
      set -l qfile (ls *.json | fuzzel -d -p 'Questions Database:')	
      if test -n "$qfile"
        mtest backup $qfile
      end
      run_gui
    case '5. Exit'
      echo 'Goodbye!'
  end
end   

function run_gui
  set -l action (printf "%s\n" '1. Profile Setup' '2. Create Exam' '3. Mark Papers' '4. Backup Exam' '5. Exit' | fuzzel -d -p 'System:')
  main_menu $action
end

run_gui
