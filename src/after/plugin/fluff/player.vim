let s:dir = expand('<script>:p:h')

function s:run(filename, play) abort
  let lyrics_path = $'{s:dir}/{a:filename}.txt'
  let mp3_path = $'{s:dir}/{a:filename}.mp3'
  if filereadable(lyrics_path)
    echo readfile(lyrics_path)->join("\n")
  endif
  if a:play
    if !filereadable(mp3_path)
      echoerr $'{mp3_path} not found'
      return
    endif
    call afplay#play(mp3_path)
  endif
endfunction

" bang to play song
command! -bang -bar -range HappyChildren call s:run('happy-children', <bang>0)
command! -bang -bar -range Blossom call s:run('blossom', <bang>0)
command! -bang -bar -range BurnedWithDesire call s:run('burned-with-desire', <bang>0)
