% Script written to change file names for EddyPro processing during times
% with incorrect time zones (e.g. UTC or PDT instead of PCT). According to
% LiCOR, only the .ghg file names need to be changed to correct the time
% stamp when processing in EddyPro.

% Written by Sara Knox (April, 2019)
pathFolder = '/Users/saraknox/Personal Content/Projects/2014-Burns Bog/Flux-tower/20Hz';

d = dir(pathFolder);
isub = [d(:).isdir]; % returns logical vector
nameFolds = {d(isub).name}';
nameFolds(ismember(nameFolds,{'.','..','uncorrected_UTC','uncorrected_PCT'})) = [];

% Define time period of interest
ts = datevec(datenum(2019,3,15,3,30,0));
te = datevec(datenum(2019,3,25,12,30,0));

% Consider only time period of interest
year_first = ts(1);
year_last = te(1);

years = str2num(cell2mat(nameFolds));

iyr = find(years >= year_first & years <= year_last);

nameFolds = nameFolds(iyr);

file_ext = {'*.ghg','*.data','*.metadata','*.status'};

% Loop through each folder of relevant years and change names under raw folder
for i = 1:length(nameFolds)
    
    % Find subfolders within each year
    pathsubFolder = [pathFolder '/' cell2mat(nameFolds(i))];
    
    d = dir(pathsubFolder);
    isub = [d(:).isdir]; % returns logical vector
    namesubFolds = {d(isub).name}';
    namesubFolds(ismember(namesubFolds,{'.','..'})) = [];
    
    % Consider only years & months of interest
    month_first = ts(2);
    month_last = te(2)+1; % Add one month for file naming structure (i.e. dates in March are included in April folder)
    
    Mdate_first = datenum(str2num(cell2mat(nameFolds(1))),month_first,1,0,0,0);
    Mdate_last = datenum(str2num(cell2mat(nameFolds(end))),month_last,1,0,0,0); 
    
    namesubFolds_num = cell2mat(namesubFolds);
    months = str2num(namesubFolds_num(:,5:6));
    Mdate_yymm = datenum(repmat(str2num(cell2mat(nameFolds(i))), length(months),1), months,1,0,0,0);
    
    iyrmm = find(Mdate_yymm >= Mdate_first & Mdate_yymm <= Mdate_last);
    
    namesubFolds = namesubFolds(iyrmm);
    
    for j = 1:length(namesubFolds)
        
        % First move folder so that original files are copied as backup
        path_orig = [pathFolder '/' cell2mat(nameFolds(i)) '/' cell2mat(namesubFolds(j))];
        path_new = [pathFolder '/uncorrected_PCT/' cell2mat(nameFolds(i)) '/' cell2mat(namesubFolds(j))];
        
        if ~exist(path_new, 'dir')
            mkdir(path_new)
        end
        
        copyfile(path_orig,path_new)
        
        % Then correct file name
        pathraw = [pathFolder '/' cell2mat(nameFolds(i)) '/' cell2mat(namesubFolds(j)) '/raw/'];
        
        % Identify all files for a given extension
        for k = 1:length(file_ext)
            
            d = dir([pathraw cell2mat(file_ext(k))]);
            
            Nfiles = length(d);
            
            % Loop through each file and change file name
            for l = 1:Nfiles
                
                fileName = d(l).name;
                
                %extract time from file name
                yyyy = str2num(fileName(1:4));
                mm = str2num(fileName(6:7));
                dd = str2num(fileName(9:10));
                HH = str2num(fileName(12:13));
                MM = str2num(fileName(14:15));
                SS = str2num(fileName(16:17));
                
                Mdate = datenum(yyyy,mm,dd,HH,MM,SS);
                
                % Change file name if within period of interest
                if Mdate >= datenum(ts) && Mdate <= datenum(te)
                    
                    t = datetime(Mdate,'ConvertFrom','datenum');
                    t_pct = t - hours(1);
                    
                    dstr_pct = datestr(t_pct,'yyyymmddHHMMSS');
                    
                    yyyy_pct = dstr_pct(1:4);
                    mm_pct = dstr_pct(5:6);
                    dd_pct = dstr_pct(7:8);
                    HH_pct = dstr_pct(9:10);
                    MM_pct = dstr_pct(11:12);
                    SS_pct = dstr_pct(13:14);
                    
                    fileName_pct = [yyyy_pct '-' mm_pct '-' dd_pct 'T' HH_pct MM_pct SS_pct '_' fileName(19:end)];
                    
                    % Rename file
                    new = fullfile(pathraw, fileName_pct);
                    old = fullfile(pathraw, fileName);
                    
                    % Then rename file
                    movefile(old,new);
                end
            end
        end
    end
end
