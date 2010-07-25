function set_on_click(section){
  $("body").mouseup(function(e){
    var offset = $(section).offset();
    window.status = e.pageX +', '+ e.pageY + "\tdiv position: " + offset.left + ", " + offset.top;
    
    var code = jQuery.trim(get_output_selected_text(section));
    if(code == ""){
      $("#run_highlight").hide();
      return;
    }

    $("#run_highlight").offset({ top: e.pageY, left: (offset.left - $("#run_highlight").width() - 20) });
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
    window.console.log("Hide output!");
    $(".R_output").hide();    
    $(".R_output_image").hide();    
//    $(".R_output").hide('slow');    
  }else{
    window.console.log("Show output!");

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


// Block the screen while executing a package demo
function execute_demo(package, demo){
  $("#run_highlight").hide();

  $.blockUI({ message: '<h1><img src="/_images/busy.gif" /> Please view the R console to advance the demo</h1>' }); 
  
  setTimeout(function(){
    jQuery.ajax({
      url: "/package/"+package+"/exec_demo/"+demo,
      success: function() {
        $.unblockUI();
        notify("The demo:" +demo+" has finished executing in the R console.");
      },
      error:function (xhr, ajaxOptions, thrownError){
        $.unblockUI();
        error_notify("The demo did not run execute properly.");
      }
    })
  }, 500);
}

function execute_example(package, topic){
   $.blockUI({ message: '<h1><img src="/_images/busy.gif" /> Please view the R console to advance the example</h1>' }); 
  
  setTimeout(function(){
    jQuery.ajax({
      url: "/package/"+package+"/topic/"+topic + "/exec_example",
      success: function() {
        $.unblockUI();
        notify("The example for " +topic+" has finished executing in the R console.");
      },
      error:function (xhr, ajaxOptions, thrownError){
        $.unblockUI();
        error_notify("The example did not run execute properly.");
      }
    })
  }, 500); 
}


function get_output_selected_text(section){
  var code = getSelText();
  window.console.log("Selected Code: \n" + code);
  if(code == "")
    return "";
  
  window.console.log("Section: "+section+"\ncode_index: "+$(section).text().indexOf(code));
  var i;
  code = "" + code;
  var lines = code.split("\n");
  for(i =0; i < lines.length; i++){
    window.console.log("Section: "+lines[i]+"\ncode_index: "+$(section).text().indexOf(lines[i])+"\noutput_index: "+$(".R_output").text().indexOf(lines[i]));
    if( $(section).text().indexOf(lines[i]) < 0) {
      message_notify("Removing: \n"+lines[i]);
      lines[i] = "";
    } else if( $(".R_output").text().indexOf(lines[i]) >= 0) {
      message_notify("Removing: \n"+lines[i]);
      lines[i] = "";
    }
  }
  
  code = lines.join("%0A");
  code = r_urlencode(code);
  window.console.log("code : " + code);
  
  return code;
  
}

// Block the screen while code is executed
function run_selected_code(section){
  
  var code = get_output_selected_text(section);

  if(code == "")
    return;
  
  $.blockUI({ message: '<h1><img src="/_images/busy.gif" /> Running selected code in the R console</h1>' }); 
  
  setTimeout(function(){
    jQuery.ajax({
      url: "/eval_text/" + code,
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
  
  $.blockUI({ message: "<h1><img src=\"/_images/busy.gif\" /> Running "+demo_topic+". Please wait.</h1>" }); 
  
  setTimeout(function(){
    jQuery.ajax({
      url: "/eval_"+demo_topic+"/" + package + "-" + demo_name,
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


