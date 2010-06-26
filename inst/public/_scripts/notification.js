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

// Block the screen while code is executed
function run_selected_code(section){

  var code = getSelText();
  window.console.log("Selected Code: \n" + code);
  if(code == "")
    return;
  
  window.console.log("Section: "+section+"\ncode_index: "+$(section).text().indexOf(code));
  if( $(section).text().indexOf(code) < 0) {
    error_notify("Select R code only.");    
    return;
  }
  
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