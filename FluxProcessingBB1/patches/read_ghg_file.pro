;+
; name:
;   read_ghg_file.pro
;
; purpose:
;   reads raw data from a LICOR ghg file into a structure.  It automatically renames 
;   the file to .zip, then unzips the file (extracts .data, .metadata, .status files).
;   Then reads the .data file, skips the header, but reads the header labels, and 
;   puts all data into a structure.
;
; category:
;   file handing
;
; calling sequence:
;   data=read_ghg_file(file)
;
; inputs:
;   file  : string. full path to a .ghg file
;   cleanup : keyword. if set cleanup=1 then all extracted files are deleted after reading.
;   
; output:
;   data  : structure. structure with data from data section of .ghg files
;           tag-names refer to header of .ghg file
;
; example
;   data = read_ghg_file('/Users/achristn/Desktop/GHG/2015-08-01T120000_AIU-0547.ghg')
;   help, data
;   p = plot(data.AUX_3__W_MS)
;   print, stddev(data.AUX_3__W_MS)
;
; revision history:
;   mar-09-2016 ac
;   apr-19-2016 ac - added functionality to return content of header in structure
;-

function read_ghg_file, file, cleanup=cleanup, exdir=exdir

  if not keyword_set(exdir) then exdir = '/Users/Shared/ghg/'
  
  temp_zipfile = exdir+file_basename(file,'.ghg')+'.zip'
  FILE_COPY, file, temp_zipfile
  
  ;FILE_UNZIP, temp_zipfile, files=unzippedfiles
  SPAWN, 'unzip '+file+' -d '+exdir
  datafile = exdir+file_basename(file,'.ghg')+'.data'
  metafile = exdir+file_basename(file,'.ghg')+'.metadata'
  statusfile = exdir+file_basename(file,'.ghg')+'.status'
  FILE_DELETE, temp_zipfile
  
  ; read data-file
  line = ''

  openr, lun, datafile, /get_lun
  header_lines = 0L
  header_end = 0B
  while eof(lun) eq 0 and header_end eq 0B do begin
    readf, lun, line
    header_lines = header_lines + 1
    if strpos(line, 'DATAH') eq 0 then begin ; read header column
      split = STRSPLIT(line, string(9B), /EXTRACT)
      nc = n_elements(split)
      names = strarr(nc)
      for c=0, nc-1 do names[c] = STRUPCASE(STRJOIN(STRSPLIT(split[c], /EXTRACT), '_'))
      names = ubc_tls_check_tagnames(names)
      header_end = 1B
    endif
  endwhile
  header = strarr(header_lines)
  point_lun, lun, 0
  for h=0, header_lines-1 do readf, lun, line
  data_lines = 0L
  while not eof(lun) do begin
    readf, lun, line
    data_lines = data_lines + 1
  endwhile
  
  ; set-up structure
  
  data = create_struct(names[nc-1],fltarr(data_lines)*!values.f_nan)
  
  for c=2, nc do begin
    if strupcase(names[nc-c]) eq 'DATAH' or strupcase(names[nc-c]) eq 'DATE' or strupcase(names[nc-c]) eq 'TIME' then begin 
      data = create_struct(names[nc-c],strarr(data_lines),data)
    endif else begin  
      if  strupcase(names[nc-c]) eq 'SECONDS' or strupcase(names[nc-c]) eq 'NANOSECONDS' then begin
        data = create_struct(names[nc-c],lonarr(data_lines),data)
      endif else begin
        data = create_struct(names[nc-c],dblarr(data_lines)*!values.f_nan,data)
      endelse
    endelse
  endfor
  
  ; read data
  
  point_lun, lun, 0
  for h=0, header_lines-1 do begin
    readf, lun, line ; skip header
    header[h] = line
  endfor
  for i=0, data_lines-1 do begin
    readf, lun, line
    split = STRSPLIT(line, string(9B), /EXTRACT)
    for c=0, nc-1 do begin
       data.(c)[i] = split[c] 
    endfor
  endfor
  
  close, lun
  free_lun, lun
  
  data = create_struct('header',header,data)
  
  ;delete extracted files if keyword "cleanup" is used
  
  if keyword_set(cleanup) then begin
    FILE_DELETE, unzippedfiles
  endif
  
  return, data
  
end