clear all

pathFolder = '/Volumes/Knox USGS/20Hz';
d = dir(pathFolder);
isub = [d(:).isdir]; % returns logical vector
nameFolds = {d(isub).name}';
nameFolds(ismember(nameFolds,{'.','..','uncorrected_UTC','uncorrected_PCT','uncorrected_sonic'})) = [];

% Define time period of interest to identify folders of interest
ts = datevec(datenum(2016,3,1,0,0,0));
te = datevec(datenum(2016,4,30,23,30,0));

% Define correction factors
% From June 10, 2015 to July 27, 2015, use correction factor [2,2,1./0.8985]
% after July 27 until April 30, 2016, use correction factor [1,1,1./0.8985]
% After April 30, 2016 no correction factor needed
velocity_multipliers = [2 2 1./0.8985;
    1,1,1./0.8985;
    1 1 1];

% Consider only time period of interest
year_first = ts(1);
year_last = te(1);

years = str2num(cell2mat(nameFolds));
iyr = find(years >= year_first & years <= year_last);
nameFolds = nameFolds(iyr);

check = 0;
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
    month_last = te(2); % Add one month for file naming structure (i.e. dates in March are included in April folder)
    
    Mdate_first = datenum(str2num(cell2mat(nameFolds(1))),month_first,1,0,0,0);
    Mdate_last = datenum(str2num(cell2mat(nameFolds(end))),month_last,1,0,0,0);
    
    namesubFolds_num = cell2mat(namesubFolds);
    months = str2num(namesubFolds_num(:,1:2));
    Mdate_yymm = datenum(repmat(str2num(cell2mat(nameFolds(i))), length(months),1), months,1,0,0,0);
    
    iyrmm = find(Mdate_yymm >= Mdate_first & Mdate_yymm <= Mdate_last);
    
    namesubFolds = namesubFolds(iyrmm);
    
    for j = 1:length(namesubFolds)
        
        % First move folder so that original files are copied as backup
        path_orig = [pathFolder '/' cell2mat(nameFolds(i)) '/' cell2mat(namesubFolds(j))];
        path_new = [pathFolder '/uncorrected_sonic/' cell2mat(nameFolds(i)) '/' cell2mat(namesubFolds(j))];
        path_new_yr = [pathFolder '/uncorrected_sonic/' cell2mat(nameFolds(i))];
        
        if ~exist(path_new_yr, 'dir')
            mkdir(path_new_yr)
        end
        
        movefile(path_orig,path_new)
        
        if ~exist(path_orig, 'dir')
            mkdir(path_orig)
        end
        
        % path for corrected files
        outpath = [path_orig '/'];
        
        % Then correct file
        fpath = [path_new '/'];
        finfo = dir([fpath '/' '*AIU-0547.*']);
        
        for k = 1:length(finfo)
            
            % consider dates of interest as outlined above
            [~, fname, ext] = fileparts(finfo(k).name);
            if strcmp(ext, '.metadata') || strcmp(ext, '.status') || strcmp(ext, '.data')
                continue;
            elseif strcmp(ext, '.ghg')
                fns = unzip([fpath fname ext], fpath);
            end
            
            % copy file to zip with .data file - CAREFUL WITH INDICES
            % IF CHANGING PATH
            dest3 = cell2mat(fns(3));
            copyfile(cell2mat(fns(3)),[dest3(1:24) dest3(43:end)])
            dest2 = cell2mat(fns(2));
            copyfile(cell2mat(fns(2)),[dest2(1:24) dest2(43:end)])
            
            fid = fopen([fpath fname '.data'],'r');
            if fid ~= -1
                
                if ~check
                    fprintf('%s\n', fname);
                    oid = fopen([outpath fname '.data'], 'w');
                end
                
                for l = 1:7
                    line = fgets(fid);
                    
                    if ~check
                        fprintf(oid, '%s', line);
                    end
                end
                head = fgets(fid);
                h = regexp(head,'\t','split');
                
                fprintf(oid, '%s', head);
                line = fgets(fid);
                
                l = 8;
                
                % Select appropriate multiplier
                if datenum(finfo(k).date) >= datenum(2015,1,1,0,30,0) && datenum(finfo(k).date) <= datenum(2015,7,27,23,30,0)
                    velocity_multiplier = velocity_multipliers(1,:);
                elseif datenum(finfo(k).date) >= datenum(2015,7,28,0,0,0) && datenum(finfo(k).date) <= datenum(2016,4,30,23,30,0)
                    velocity_multiplier = velocity_multipliers(2,:);
                else
                    velocity_multiplier = velocity_multipliers(3,:);
                end
                
                while line ~= -1
                    l = l + 1;
                    d = regexp(line,'\t','split');
                    
                    % correct u, v, w
                    % find indices
                    iu = strcmp(h,'Aux 1 - U (m/s)');
                    iv = strcmp(h,'Aux 2 - V (m/s)');
                    iw = strcmp(h,'Aux 3 - W (m/s)');
                    
                    % extract wind speeds
                    u = str2num(cell2mat(d(iu)));
                    v = str2num(cell2mat(d(iv)));
                    w = str2num(cell2mat(d(iw)));
                    
                    % correct wind speeds using correction factors
                    if u ~= -9999
                        u_corr = u*velocity_multiplier(1);
                        v_corr = v*velocity_multiplier(2);
                        w_corr = w*velocity_multiplier(3);
                    end
                    
                    d2 = d;
                    d2(iu) = cellstr(num2str(u_corr,'%0.6g'));
                    d2(iv) = cellstr(num2str(v_corr,'%0.6g'));
                    d2(iw) = cellstr(num2str(w_corr,'%0.6g'));
                    line2 = strjoin(d2,'\t');
                    
                    fprintf(oid, '%s', line2);
                    clear line2 d2;
                    line = fgets(fid);
                end
            end
            
            fclose(fid);
            fclose(oid);
            
            if exist('fns', 'var')
                for m = 1:length(fns)
                    %delete unzipped file
                    delete(fns{m});
                end
                clear fns;
            end
            
            % rezip files
            zip([outpath fname '.ghg'], {[outpath fname '.data'],[outpath fname '.status'],[outpath fname '.metadata']});
            
            % rename to get rif of .zip extension
            movefile([outpath fname '.ghg.zip'],[outpath fname '.ghg'])
            
            % delete unzipped files
            delete([outpath fname '.data']);
            delete([outpath fname '.metadata']);
            delete([outpath fname '.status']);
        end
    end
end
