function notify(function_text, start_text, end_text){
  
  jQuery.ajax({
    url: "/packages/:package/exec_demo/:demo",
    dataType: "json",
    success: function(packages) {
      for(i = 0; i < packages.length; i++) {
        pkg = packages[i];
        $("#" + pkg).removeClass("old").addClass("update");
      }
      set_update_button();
    }
  })

  
}

//http://awgy.net/achtung/demo/
var notice_settings = {
  timeout: 3,
//  className: 'achtungSuccess',
  icon: 'ui-icon-check'
}


function tell_highlighted(){
  alert ($().selectedText());
}

function notify(text)
{
    $.achtung(notice_settings, {
        message: text
    });
}  

//http://www.codetoad.com/javascript_get_selected_text.asp
function getSelText()
{
  var txt = '';
  if (window.getSelection){
    txt = window.getSelection();
  } else if (document.getSelection) {
    txt = document.getSelection();
  }else if (document.selection){
    txt = document.selection.createRange().text;
  }else
   return;
  
  window.console.log("Selected Text:\n\t"+txt);
  return txt;
}


//http://jquery.malsup.com/block/#page
function execute_demo(package, demo){
  
  $.blockUI({ message: '<h1><img src="/_images/busy.gif" /> Please view the R console to advance the demo</h1>' }); 
  
  jQuery.ajax({
    url: "/packages/"+package+"/exec_demo/"+demo,
    success: function() {
      $.unblockUI();
      notify("The demo:" +demo+" has finished executing in the R console.");
      
    }
  })
 
}

function run_selected_code(){
  $.blockUI({ message: '<h1><img src="/_images/busy.gif" /> Running selected code in the R console</h1>' }); 

  var code = getSelText();

  jQuery.ajax({
    url: "/eval_text/"+code,
    dataType: "json",
    success: function() {
      $.unblockUI();
      notify("The highlighted selection has finished executing in the R console.");      
    }
  })
}


$('#demo_source_code').click(function() {
  alert("You selected: " + $.selectedText());
});

