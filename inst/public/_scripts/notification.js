function set_on_click(section){
  
  $("#bd").mouseup(function(e){
    
    var offset = $(section).offset();
    o.log(e.pageX +', '+ e.pageY + "\tdiv position: " + offset.left + ", " + offset.top);
    
    var code = $.trim(get_output_selected_text(section));
    if (! code ) {
      $("#run_highlight").hide();
      return;
    }

    // $("#run_highlight").offset({ top: e.pageY, left: (offset.left - $("#run_highlight").width() - 20) });
    $("#run_highlight").show('slow');
  }); 
}



function r_urlencode (str) {
  str = escape(str);
  return str.replace(/[*+\/@]|%20/g,
    function (s) {
      switch (s) {
        case "*": s = "%2A"; break;
        case "+": s = "%2B"; break;
        case "/": s = "%2F"; break;
        case "@": s = "%40"; break;
      }
      return s;
    }
  );
}



// hide/show the R output 
var has_shown_output = 0;
var output_hidden = 1;
function hide_show_output(demo_topic, package, text_name){
  $("#run_highlight").hide();

  if(output_hidden == 0){
    o.log("Hide output!");
    $(".R_output").hide();    
    $(".R_output_image").hide();    
//    $(".R_output").hide('slow');    
  }else{
    o.log("Show output!");

    if(has_shown_output == 0){
      evaluate_section(demo_topic, package, text_name)
      has_shown_output = 1;
    }else{
      $(".R_output").show('slow');    
      $(".R_output_image").show('slow'); 
    }
  }
  output_hidden = (output_hidden+1) % 2;  
}


// Notification:
//   http://awgy.net/achtung/demo/

// Block Screen:
//   http://jquery.malsup.com/block/#page

// Produces black with checkmark
var notice_settings = {
  timeout: 3,
  className: 'achtungSuccess',
  icon: 'ui-icon-check'
}

// Wrapper to notify user with text
function notify(text){
    $.achtung(notice_settings, {
        message: text
    });
}  

// Produces red with error symbol
var error_settings = {
  timeout: 3,
  className: 'achtungFail',
  icon: 'ui-icon-alert'
}

// Wrapper to notify user of error
function error_notify(text){
    $.achtung(error_settings, {
        message: text
    });
}  

function message_notify(text){
    $.achtung({timeout: 3, icon: 'ui-icon-alert'}, {
        message: text
    });
}  
 



// Retrive the selected text on the page
// http://www.codetoad.com/javascript_get_selected_text.asp
function getSelText()
{
  if (window.getSelection){
    return window.getSelection();
  } else if (document.getSelection) {
    return document.getSelection();
  }else if (document.selection){
    return document.selection.createRange().text;
  }else
   return;
}



function get_output_selected_text(section){
  var code = getSelText() + "";
  o.log("Selected Code: \n" + code);
  if(code == "")
    return "";
  
  o.log("Section: "+section+"\ncode_index: "+$(section).text().indexOf(code));
  var i;
  code = "" + code;
  var lines = code.split("\n");
  for(i =0; i < lines.length; i++){
    o.log("Section: "+lines[i]+"\ncode_index: "+$(section).text().indexOf(lines[i])+"\noutput_index: "+$(".R_output").text().indexOf(lines[i]));
    if( $(section).text().indexOf(lines[i]) < 0) {
      // message_notify("Removing: \n"+lines[i]);
      lines[i] = "";
    } else if( $(".R_output").text().indexOf(lines[i]) >= 0) {
      // message_notify("Removing: \n"+lines[i]);
      lines[i] = "";
    }
  }
  
  code = lines.join("%0A");
  code = r_urlencode(code);
  o.log("code : " + code);
  
  return code;
  
}

// Block the screen while code is executed
function run_selected_code(section){
  
  var code = get_output_selected_text(section);

  if(code == "")
    return;
  
  $.blockUI({ message: '<h1><img src="' + routerUrl + '/_images/busy.gif" /> Running selected code in the R console</h1>' }); 
  
  setTimeout(function(){
    jQuery.ajax({
      url: routerUrl + "/eval_text/" + code,
      dataType: "json",
      success: function() {
        $.unblockUI();
        notify("The highlighted selection has finished executing in the R console.");
      },
      error:function (xhr, ajaxOptions, thrownError){
        $.unblockUI();
        error_notify("The code that was selected did not run execute properly.");
      }
    })
  }, 500);
}



function evaluate_section(demo_topic, package, demo_name){
  
  $.blockUI({ message: "<h1><img src=\"" + routerUrl + "/_images/busy.gif\" /> Running "+demo_topic+". Please wait.</h1>" }); 
  
  setTimeout(function(){
    jQuery.ajax({
      url: routerUrl + "/eval_"+demo_topic+"/" + package + "~" + demo_name,
      dataType: "html",
      success: function(eval_code) {
        $("#"+demo_topic+"_source_code").html(eval_code)
        $.unblockUI();
        
      },
      error:function (xhr, ajaxOptions, thrownError){
        $.unblockUI();
        error_notify("The "+demo_topic+" did not execute properly.");
      }
    })
  }, 500);
  
}

