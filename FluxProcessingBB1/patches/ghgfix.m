clear all

% fpath = 'J:\FieldData\ShermanWetland\';
% % fpath = 'J:\FieldData\ShermanWetland\SW_2017\SW_LicorGHG_201710\';
% finfo = dir([fpath '*AIU-1569.*']);
% outpath = 'J:\FieldData\ShermanWetland\Fixed\';

% fpath = 'J:\FieldData\Mayberry\';
% % fpath = 'J:\FieldData\Mayberry\MB_2017\MB_LicorGHG_201710\';
% finfo = dir([fpath '*MB-1285.*']);
% outpath = 'J:\FieldData\Mayberry\Fixed\';

% fpath = 'J:\FieldData\WestPond\';
% % fpath = 'J:\FieldData\WestPond\WP_2017\WP_LicorGHG_201710\';
% finfo = dir([fpath '*WP-1570.*']);
% outpath = 'J:\FieldData\WestPond\Fixed\';

% fpath = 'J:\FieldData\BouldinAlfalfa\tofix\';
% finfo = dir([fpath '*AIU-1568.*']);
% outpath = 'J:\FieldData\BouldinAlfalfa\fixed\';

fpath = 'J:\FieldData\EastEnd\';
finfo = dir([fpath '*EE-0397.*']);
outpath = 'J:\FieldData\EastEnd\fixed\';

% fpath = 'J:\FieldData\BouldinCorn\';
% fpath = 'G:\FieldData\BouldinCorn\BC_2017\BC_LicorGHG_201708\';
% finfo = dir([fpath '*AIU-1570.*']);
% outpath = 'J:\FieldData\BouldinCorn\Fixed\';

% badlen = 33;
hlen = 22;
% dlen = badlen;
% fixhead = 25;
% fixhead2 = 24;

check = 0;

for i = 1:length(finfo)
    [~, fname, ext] = fileparts(finfo(i).name);
    if strcmp(ext, '.meta')
        continue;
    elseif strcmp(ext, '.ghg')
        fns = unzip([fpath fname ext], fpath);
    end
    
    fid = fopen([fpath fname '.data'],'r');
    if fid ~= -1
        flag = 1;
        
        if ~check
           fprintf('%s\n', fname);
           oid = fopen([outpath fname '.data'], 'w');
        end
        
        for j = 1:7
            line = fgets(fid);
            if ~check
                fprintf(oid, '%s', line);
            end
        end
        head = fgets(fid);
        h = regexp(head,'\t','split');
        if check
            line = fgets(fid);
            d = regexp(line,'\t','split');
            fprintf('%s, %d, %d\n', fname, length(h), length(d));
            fclose(fid);
            if exist('fns', 'var')
                for j = 1:length(fns)
                    %delete unzipped file
                    delete(fns{j});
                end
                clear fns;
            end
            continue;
        end
        
        if length(h) == 24
            n = regexp(head,'\t');
            head = [head(1:n(15)) head(n(17)+1:end)];
        elseif length(h) == 21
            n = regexp(head,'\t');
            head = [head(1:n(4)) head(n(5)+1:end)];
        elseif length(h) == 23
            n = regexp(head,'\t');
            head = [head(1:n(4)) head(n(5)+1:end)];
        elseif length(h) == 25
            n = regexp(head,'\t');
            head = [head(1:n(4)) head(n(5)+1:n(16)) head(n(18)+1:end)];
        elseif length(h) == 27
            n = regexp(head,'\t');
            head = [head(1:n(15)) head(n(20)+1:end)];
%         elseif length(h) == 27
%             n = regexp(head,'\t');
%             head = [head(1:n(4)) head(n(5)+1:n(16)) head(n(20)+1:end)];
%         elseif length(h) == 27
%             n = regexp(head,'\t');
%             head = [head(1:n(15)) head(n(20)+1:n(27))];
        elseif length(h) == 28
            n = regexp(head,'\t');
            head = [head(1:n(16)) head(n(21)+1:end)];
%         elseif length(h) == 29
%             n = regexp(head,'\t');
%             head = [head(1:n(4)) head(n(5)+1:n(14)) head(n(15)+1:n(19)) head(n(26)+1:end)];
       elseif length(h) == 29
            n = regexp(head,'\t');
            head = [head(1:n(5)) head(n(7)) head(n(8)+1:n(11)) head(n(13)) head(n(15)) head(n(17)) head(n(18)+1:n(22)) head(n(23)+1:end)];
        elseif length(h) == 30
            n = regexp(head,'\t');
            head = [head(1:n(15)) head(n(16)+1:n(20)) head(n(27)+1:end)];
%         elseif length(h) == 31
%             n = regexp(head,'\t');
%             head = [head(1:n(4)) head(n(5)+1:n(16)) head(n(17)+1:n(21)) head(n(28)+1:end)];
        elseif length(h) == 31
            n = regexp(head,'\t');
            head = [head(1:n(4)) head(n(5)+1:n(13)) head(n(15)+1:n(16)) head(n(17)+1:n(21)) head(n(28)+1:end)];
%         elseif length(h) == 32
%             n = regexp(head,'\t');
%             head = [head(1:n(15)) head(n(18)+1:n(22)) head(n(29)+1:end)];
        elseif length(h) == 32
            n = regexp(head,'\t');
            head = [head(1:n(4)) head(n(5)+1:n(16)) head(n(25)+1:end)];
        elseif length(h) == 33
            n = regexp(head,'\t');
            head = [head(1:n(4)) head(n(5)+1:n(16)) head(n(19)+1:n(23)) head(n(30)+1:end)];
        elseif length(h) == 35
            n = regexp(head,'\t');
            head = [head(1:n(15)) head(n(21)+1:n(25)) head(n(32)+1:end)];
%         elseif length(h) == 35
%             n = regexp(head,'\t');
%             head = [head(1:n(4)) head(n(5)+1:n(16)) head(n(21)+1:n(25)) head(n(32)+1:end)];
        end
        h = regexp(head,'\t','split');

        if length(h) == hlen
            fprintf(oid, '%s', head);
            line = fgets(fid);            

            k = 8;
            while line ~= -1
                k = k + 1;
                if ~isempty(find(line == 't', 1))
                    line = regexprep(line, 't', '\t');
                end
                d = regexp(line,'\t','split');
                dlen = length(d);
                if dlen == 24
                    n = regexp(line,'\t');
                    line2 = [line(1:n(15)) line(n(17)+1:end)];
                elseif dlen == 21
                    n = regexp(line,'\t');
                    line2 = [line(1:n(4)) line(n(5)+1:end)];
                elseif dlen == 25
                    n = regexp(line,'\t');
                    line2 = [line(1:n(4)) line(n(5)+1:n(16)) line(n(18)+1:end)];
                elseif dlen == 27
                    n = regexp(line,'\t');
                    line2 = [line(1:n(15)) line(n(20)+1:end)];
                elseif dlen == 28
                    n = regexp(line,'\t');
                    line2 = [line(1:n(16)) line(n(20)+1:end)];
                elseif dlen == 29
                    n = regexp(line,'\t');
                    line2 = [line(1:n(4)) line(n(5)+1:n(14)) line(n(15)+1:n(19)) line(n(26)+1:end)];
                elseif dlen == 30
                    n = regexp(line,'\t');
                    line2 = [line(1:n(15)) line(n(16)+1:n(20)) line(n(27)+1:end)];
                elseif dlen == 31
                    n = regexp(line,'\t');
                    line2 = [line(1:n(4)) line(n(5)+1:n(16)) line(n(17)+1:n(21)) line(n(28)+1:end)];
                elseif dlen == 32
                    n = regexp(line,'\t');
                    line2 = [line(1:n(15)) line(n(18)+1:n(22)) line(n(29)+1:end)];
                elseif dlen == 33
                    n = regexp(line,'\t');
                    line2 = [line(1:n(4)) line(n(5)+1:n(16)) line(n(19)+1:n(23)) line(n(30)+1:end)];
                elseif dlen == 35
                    n = regexp(line,'\t');
                    line2 = [line(1:n(15)) line(n(21)+1:n(25)) line(n(32)+1:end)];
%                 elseif dlen == 35
%                     n = regexp(line,'\t');
%                     line2 = [line(1:n(4)) line(n(5)+1:n(16)) line(n(21)+1:n(25)) line(n(32)+1:end)];
                elseif dlen == 40
                    n = regexp(line,'\t');
                    line2 = [line(1:n(4)) line(n(5)+1:n(16)) line(n(26)+1:n(30)) line(n(37)+1:end)];
%                 elseif length(d) == 19
%                     line = regexprep(line, 't', '\t');
%                     d = regexp(line,'\t','split');
%                     if length(d) == 22
%                         fprintf('fixed bad delims line %d\n', k);
%                         dlen = fixhead;
%                     else
%                         fprintf('bad line found at %d\n', k);
%                         fprintf('%s', line);
%                         flag = 0;
%                         break;
%                     end
                elseif dlen == 22
                    line2 = line;
                else
                    fprintf('Unknown data length: %d\n', dlen);
                    fprintf('%s\n%s', head, line);
                    flag = 0;
                    break;
                end
                
                d = regexp(line2,'\t','split');
                if length(d) ~= 22
                    fprintf('Bad header length: expect %d, got %d\n', hlen, length(d));
                    flag = 0;
                    break;
                end
                
                fprintf(oid, '%s', line2);
                clear line2;
                line = fgets(fid);
            end
        else
            fprintf('Bad header length: expect %d, got %d\n', hlen, length(h));
            flag = 0;
        end
                
    end
    
    fclose(fid);
    fclose(oid);
    if exist('fns', 'var')
        for j = 1:length(fns)
            %delete unzipped file
            delete(fns{j});
        end
        clear fns;
    end
        
    if flag == 0
        break;
    end
    
    zip([outpath fname '.ghg'], [outpath fname '.data']);
    delete([outpath fname '.data']);
    
end

%                         if length(d) ~= dlen
%                     if length(d) == hlen
%                         fprintf('Setting data length to header length: %d\n', hlen);
%                         dlen = hlen;
%                     elseif length(d) == badlen
%                         fprintf('Resetting to bad length: %d\n', badlen);
%                         dlen = badlen;
%                     elseif length(d) == fixhead
%                         fprintf('Setting date length to fixhead: %d\n', fixhead);
%                         dlen = fixhead;
%                     elseif length(d) == fixhead2
%                         fprintf('Setting date length to fixhead: %d\n', fixhead2);
%                         dlen = fixhead2;
%                     elseif length(d) == 19
%                         line = regexprep(line, 't', '\t');
%                         d = regexp(line,'\t','split');
%                         if length(d) == fixhead
%                             fprintf('fixed bad delims line %d\n', k);
%                             dlen = fixhead;
%                         else
%                             fprintf('bad line found at %d\n', k);
%                             fprintf('%s', line);
%                             flag = 0;
%                             break;
%                         end
%                     else
%                         fprintf('Bad data length: expect %d, got %d at line %d\n', dlen, length(d), k);
%                         fprintf('%s\n%s', head, line);
%                         fprintf('header length is %d\n', hlen);
%                         str = sprintf('Set dlen to %d (y/n/q): ', length(d));
%                         yn = input(str, 's');
%                         if yn == 'q'
%                             flag = 0;
%                             break;
%                         elseif yn == 'y'
%                             dlen = length(d);
%                         else
%                             break;
%                         end
%                     end
%                 end
%                 
%                 fprintf(oid, '%s', line2);
%                 clear line2;
%                 line = fgets(fid);
%             end
%         else
%             fprintf('Bad header length: expect %d, got %d\n', hlen, length(h));
%             flag = 0;
%         end
% 