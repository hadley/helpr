function highlight_old_packages()
{
  // start spinning
  jQuery.ajaxSync({
  	url: "/ajax/old_packages.html",
  	success: function(html)
  	{
//  	  alert(html);
  	  window.console.log(html);
  	  
  	  var old_packs = JSON.parse(html);
  	  var i;
  	  
  	  for(i = 0; i < old_packs.length; i++){
  	    var row = document.getElementById("" + old_packs[i].Package);
  	    if(row != null){
    	    row.className = "out_of_date";
  	    }
  	  }
  	  
  	  // stop spinning
  	}
  });
}

