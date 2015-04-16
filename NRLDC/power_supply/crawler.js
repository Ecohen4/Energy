/*
In browser site crawler (created for Elliot's energy project)


1. Run this in the containing folder
---------------------------------------
python -m SimpleHTTPServer


2. Visit site in browser
---------------------------------------
http://www.nrldc.org/doold_format.aspx


3. Save & Run this bookmarklet
---------------------------------------
javascript:(function(){
    [
        'http://code.jquery.com/jquery.js',
        'http://localhost:8000/crawler.js'
    ].forEach(function(src) {
        var script = document.createElement('script');
        script.src = src;
        script.async = false;
        document.head.appendChild(script);
    });

}());


4. View output here (and in console)
----------------------------------------
filesystem:http://www.nrldc.org/temporary/


More information
-------------------------------------------
http://www.html5rocks.com/en/tutorials/file/filesystem/
http://stackoverflow.com/questions/11676584/where-does-persistent-file-system-storage-store-with-chrome

*/


$(function(){
    var postData = {
        __VIEWSTATE: $('#__VIEWSTATE').val(),
        __EVENTVALIDATION: $('#__EVENTVALIDATION').val(),
        Button1: 'Get Report of Previous Days'
    };

    function crawl(){
        var startDate = new Date(2011, 0, 1);
        var endDate = new Date();
        var dates = dateRange(startDate, endDate);

        var deferred = $.Deferred().resolve();
        $.each(dates, function(i, date){
            deferred = deferred.then(function(){
                return parsePage(date);
            });
        });
    }
    initFileSystem();
    crawl();

    function parsePage(date){
        postData.txtStartDate = date;
        return $.post('', postData, function(html){
            // Update viewstate
            postData.__VIEWSTATE = $(html).find('#__VIEWSTATE').val();
            postData.__EVENTVALIDATION = $(html).find('#__EVENTVALIDATION').val();

            $(html).find('#Panel1 table')
                .each(function(i){
                    var $table = $(this);
                    var csv = parseTable($table, date);
                    var fileName = 'table' + (i + 1) + '.csv';
                    writeFile(fileName, csv);
                });
            console.log(date);
        });
    }


    function dateRange(startDate, endDate){
        var date = startDate;
        var dates = [];
        while (date < endDate){
            date = new Date(date);
            dates.push(formatDate(date));
            date.setDate(date.getDate() + 1);
        }
        return dates;
    }

    function formatDate(date){
        return (date.getDate() < 10 ? '0' : '') + date.getDate() + '-' +
            (date.getMonth() < 9 ? '0' : '') + (date.getMonth() + 1) + '-' + 
            date.getFullYear();
    }

    function fileErrorHandler(e){
        for (var prop in FileError){
            if (FileError[prop] == e.code){
                console.log('Error:', prop);
            }
        }
    }


    var fileSystem;
    function initFileSystem(){
        window.requestFileSystem = window.requestFileSystem || window.webkitRequestFileSystem;
        window.requestFileSystem(window.TEMPORARY,
            50*1024*1024, // 20MB
            function init(fs){
                fileSystem = fs;
            }, fileErrorHandler);        
    }

    function writeFile(fileName, str){
        fileSystem.root.getFile(fileName, {create: true}, function(fileEntry){
            // Create a FileWriter object for our FileEntry (log.txt).
            fileEntry.createWriter(function(fileWriter){
                fileWriter.onwriteend = function(e) {
                console.log('Write completed.');
                };

                fileWriter.onerror = function(e) {
                console.log('Write failed: ' + e.toString());
                };

                // Start write position at EOF.
                fileWriter.seek(fileWriter.length);

                // Create a new Blob and write it to log.txt.
                var blob = new Blob([str], {type: 'text/plain'});

                fileWriter.write(blob);
            }, fileErrorHandler);
        });
    }

    function parseTable($table, date){
        // Find dimensions of matrix
        var n_rows = $table.find('tr').length;
        var n_cols = 0;

        $table.find('tr').each(function(){
            var n_tds = $(this).find('td').length;
            if (n_cols < n_tds)
                n_cols = n_tds;
        });

        // Construct matrix
        var matrix = [];
        for (var i=0; i < n_rows; i++){
            var row = [];
            for (var j=0; j < n_cols; j++){
                row[j] = null;
            }
            matrix[i] = row;
        }

        // Fill in matrix with table values
        $table.find('tr').each(function(row_index){
            var col_index = 0;
            $(this).find('td').each(function(){
                // Advance col_index to skip previously filled in matrix columns
                for (; matrix[row_index][col_index] !== null; col_index++){}

                // Fill in the matrix with the td's value
                for (var row=row_index; row < (row_index + this.rowSpan); row++){
                    for (var col=col_index; col < (col_index + this.colSpan); col++){
                        var value = $(this).text().replace('\n', '');
                        matrix[row][col] = $.trim(value);                        
                    }
                }
            });
        });

        var csv = '';
        for (var row, i=0; row=matrix[i]; i++){
            // Add date to beginning of each row
            csv += date + ',';
            csv += row.join(',') + '\n';
        }
        return csv;
    }
});