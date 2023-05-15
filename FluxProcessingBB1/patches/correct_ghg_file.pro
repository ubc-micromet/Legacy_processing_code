;+
; name:
;   correct_ghg_file.pro
;
; purpose:
;   modifies a licro ghg file and applies factors to the velocity components to correct
;   for incorrectly attributed conversions. writes a new, corrected, ghg file into the
;   directory specified as "output_directory" with the same file name, but the extension
;   "-corrected".
;
; category:
;   file handling and manipulation
;
; calling sequence:
;   correct_ghg_file, files, velocity_multipliers=velocity_multipliers, $
;      output_directory=output_directory, temporary_directory=temporary_directory
;
; inputs:
;   files  : string or string arraw of full path to all .ghg files to be imported
;            and converted, order of files does not matter
;   velocity_multipliers : float or double array [3], where [0] is multiplier for
;            u wind component, [1] is multiplier for v wind component and [2] is
;            multiplier for w wind component.
;   output_directory: string. directory to which corrected files should be written.
;   temporary_directory: a temporary, empty directory, to which temporay files are
;            written.
;            
; output:
;   new files in location of 'output_directory' with same name but extension 
;   '-corrected'
;
; example
;   ;correct w component during Burns Bog experiment
;   correct_ghg_file, '/Users/achristn/Desktop/2015-08-01T120000_AIU-0547.ghg', velocity_multipliers=[1,1,1./0.8985]
;   ;correct u,v, and w component during Summer 2015 Burns bog
;   correct_ghg_file, '/Users/achristn/Desktop/2015-07-01T120000_AIU-0547.ghg', velocity_multipliers=[2,2,1./0.8985]
;
; revision history:
;   mar-11-2016 ac
;-

pro correct_ghg_file, files, velocity_multipliers=velocity_multipliers, output_directory=output_directory, temporary_directory=temporary_directory, make_w_zero=make_w_zero
 
  if not keyword_set(output_directory) then output_directory = '/Users/Shared/Corrected ghg/'
  if not keyword_set(temporary_directory) then temporary_directory = '/Users/Shared/ghg/' ; to place files before compressing, make sure no other .ghg files are in this directory
  
  for f=0L, n_elements(files)-1 do begin
  
  temporary_ghg_file = temporary_directory+file_basename(files[f])
  file_copy, files[f], temporary_ghg_file, /overwrite
  
  ;------------------------------------
  ; read original file
  ;------------------------------------
  
  data = read_ghg_file(temporary_ghg_file)
  
  datafile = file_search(temporary_directory+'*.data')
  metafile = file_search(temporary_directory+'*.metadata')
  statusfile = file_search(temporary_directory+'*.status')
  
  if n_elements(datafile) eq 1 and n_elements(metafile) eq 1 and n_elements(statusfile) eq 1 then begin
  
  ;------------------------------------
  ; read and correct meta data file
  ;------------------------------------
;  
;  meta = ubc_fil_ascii2string(metafile)
;  ;for i=0, 510-1 do print, meta[i]
;  
;  linepos = where(strpos(meta,'instr_3_sn=unknown sn') eq 0, fcnt)
;  if fcnt eq 1 then meta[linepos[0]] = 'instr_3_sn=TG1-0190' else message, 'Error: Expression "instr_3_sn=unknown sn" not found.', /informational
;  
;  linepos = where(strpos(meta,'instr_3_id=unknown instrument') eq 0, fcnt)
;  if fcnt eq 1 then meta[linepos[0]] = 'instr_3_id=TG1-0190' else message, 'Error: Expression "instr_3_id=unknown instrument" not found.', /informational
;  
;  linepos = where(strpos(meta,'instr_3_sw_version=0.0.0') eq 0, fcnt)
;  if fcnt eq 1 then meta[linepos[0]] = 'instr_3_sw_version=1.0.23' else message, 'Error: Expression "instr_3_sw_version=0.0.0" not found.', /informational
;  
;  meta = meta + string(13B)
;  
;  ubc_fil_string2ascii, meta, metafile
;  
  ;------------------------------------
  ; read and correct status file
  ;------------------------------------
;  
;  status = ubc_fil_ascii2string(statusfile)
;  ;for i=0, 510-1 do print, status[i]
;  
;  status[0] = 'Model:  LI-7700'
;  status[1] = 'SN:  TG1-0190'
;  status[2] = 'Instrument:  TG1-0190'
;  status[4] = 'Software Version:  1.0.23'
;  status[7] = 'DATASTATH  MSEC  SECONDS NANOSECONDS DIAG  RSSI  REFRSSI LCTSETPT  LCTACTUAL BCTSETPT  BCTACTUAL CHASSISTEMP OPTICSTEMP  OPTICSRH  AUXREFTEMP  MOTORSETPT  MOTORACTUAL USB USBCAPACITY USBFREESPACE  REF GND OPTICSTDELTA  BOTTOMHEATERW TOPHEATERW  CHK'
;  
;    linepos = where(strpos(status,'Model: unknown instrument') eq 0, fcnt)
;  if fcnt eq 1 then status[linepos[0]] = 'Model:  LI-7700' else message, 'Error: Expression "Model:  unknown instrument" not found.', /informational
;    linepos = where(strpos(status,'SN:  unknown sn') eq 0, fcnt)
;  if fcnt eq 1 then status[linepos[0]] = 'SN:  TG1-0190' else message, 'Error: Expression "SN:  unknown sn" not found.', /informational
;    linepos = where(strpos(status,'Instrument:  unknown instrument') eq 0, fcnt)
;  if fcnt eq 1 then status[linepos[0]] = 'Instrument:  TG1-0190' else message, 'Error: Expression "Instrument:  unknown instrument" not found.', /informational
;    linepos = where(strpos(status,'Software Version:  0.0.0') eq 0, fcnt)
;  if fcnt eq 1 then status[linepos[0]] = 'Software Version:  1.0.23' else message, 'Error: Expression "Software Version:  0.0.0" not found.', /informational
;  
;  
;  status = status + string(13B)
;  ubc_fil_string2ascii, status, statusfile


  ;------------------------------------
  ; make w zero to account for floating input
  ;------------------------------------
  
  if keyword_set(make_w_zero) then data.AUX_3__W_MS = data.AUX_3__W_MS - mean(data.AUX_3__W_MS)
  
  ;------------------------------------
  ; apply correction as indicated by multipliers
  ;------------------------------------
  
  if velocity_multipliers[0] ne 1 then data.AUX_1__U_MS = velocity_multipliers[0] * data.AUX_1__U_MS
  if velocity_multipliers[1] ne 1 then data.AUX_2__V_MS = velocity_multipliers[1] * data.AUX_2__V_MS
  if velocity_multipliers[2] ne 1 then data.AUX_3__W_MS = velocity_multipliers[2] * data.AUX_3__W_MS
  
  ;------------------------------------
  ; delete current data file and overwrite with corrected data file
  ;------------------------------------
  
  FILE_DELETE, datafile
  
  n_rows = n_elements(data.DATAH)
  n_headers = n_elements(data.header)
  
  openw, lun, datafile, /get_lun
  for h=0L, n_headers-1 do begin
    printf, lun, data.header[h]
  endfor
  
  for r=0L, n_rows-1 do begin
    printf, lun, strjoin(strcompress([data.datah[r], $
      string(long(data.seconds[r])), $
      string(long(data.nanoseconds[r])), $
      string(long(data.DIAGNOSTIC_VALUE[r])),$
      string(long(data.DIAGNOSTIC_VALUE_2[r])),$
      data.date[r],$
      data.time[r],$
      string(double(data.CO2_MMOLM3[r]),format='(f10.4)'),$
      string(double(data.H2O_MMOLM3[r]),format='(f10.3)'),$
      string(double(data.TOTAL_PRESSURE_KPA[r]),format='(f10.3)'),$
      string(double(data.AUX_1__U_MS[r]),format='(f15.7)'),$
      string(double(data.AUX_2__V_MS[r]),format='(f12.6)'),$
      string(double(data.AUX_3__W_MS[r]),format='(f12.6)'),$
      string(double(data.AUX_4__SOS_MS[r]),format='(f10.3)'),$
      string(double(data.VIN_SMARTFLUX_V[r]),format='(f10.2)'),$
      string(double(data.CO2_UMOLMOL[r]),format='(f10.3)'),$
      string(double(data.CO2_DRYUMOLMOL[r]),format='(f10.3)'),$
      string(double(data.H2O_DRYMMOLMOL[r]),format='(f11.4)'),$
      string(double(data.CELL_TEMPERATURE_C[r]),format='(f11.4)'),$
      string(double(data.TEMPERATURE_IN_C[r]),format='(f11.4)'),$
      string(double(data.TEMPERATURE_OUT_C[r]),format='(f11.4)'),$
      string(double(data.AVERAGE_SIGNAL_STRENGTH[r]),format='(f11.4)'),$
      string(double(data.CO2_SIGNAL_STRENGTH[r]),format='(f11.4)'),$
      string(double(data.H2O_SIGNAL_STRENGTH[r]),format='(f10.3)'),$
      string(double(data.DELTA_SIGNAL_STRENGTH[r]),format='(f11.4)'),$
      string(double(data.FLOW_RATE_LPM[r]),format='(f11.4)'),$
      string(double(data.CH4_UMOLMOL[r]),format='(f11.5)'),$
      string(double(data.CH4_MMOLM3[r]),format='(f15.7)'),$
      string(double(data.CH4_TEMPERATURE[r]),format='(f10.4)'),$
      string(double(data.CH4_PRESSURE[r]),format='(f10.3)'),$
      string(double(data.CH4_SIGNAL_STRENGTH[r]),format='(f10.4)'),$
      string(long(data.CH4_DIAGNOSTIC_VALUE[r])),$
      string(long(data.CHK[r]))$
      ],/remove_all),string(9B))
  endfor
  close, lun
  free_lun, lun
  
  FILE_DELETE, temporary_ghg_file
  
  corrected_ghg_filename = output_directory + file_basename(files[f],'.ghg') + '-corrected.ghg'
  cd, temporary_directory
  SPAWN, 'zip -r -X '+corrected_ghg_filename+' '+'*.*'
  
  
  ; delete all temparay files
  FILE_DELETE, [datafile,metafile,statusfile]
  
  
  endif else begin
    
    message, 'ERROR: Multiple .ghg files found in '+temporary_directory
    
  endelse
  
  endfor
 
end