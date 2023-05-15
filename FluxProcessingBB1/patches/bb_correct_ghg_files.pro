pro BB_correct_summer_ghg_files

  ; files in summer 2015 need to be corrected for an incorrect setting on Smartflux. 
  ; We were running the CSAT3 in high mode but used incorrectly the coefficients 
  ; for low mode. A factor of 2 needs to be applied to the u and v wind components.
  ; June 10, 2015 to July 27, 2015 23:30
  
  summer_files = dialog_pickfile(path = '/Volumes/Archive/2014-Burns-Bog-ghg/', title = 'Select files from Summer 2015 with incorrect high/low setting', /multiple)
  correct_ghg_file, summer_files, velocity_multipliers=[2,2,1./0.8985], output_directory='/Users/Shared/output/', temporary_directory='/Users/Shared/ghg/', /make_w_zero
  
end

pro BB_correct_other_ghg_files

  rest_of_files = dialog_pickfile(path = '/Volumes/Archive/2014-Burns-Bog-ghg/', title = 'Select all other files (excluding Summer 2015) with incorrect w only', /multiple)
  correct_ghg_file, rest_of_files, velocity_multipliers=[1,1,1./0.8985], output_directory='/Users/Shared/output/', temporary_directory='/Users/Shared/ghg/', /make_w_zero

end