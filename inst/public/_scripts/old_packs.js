
function highlight_old_packages()
{
  // start spinning
  var out_of_date_butto = document.getElementById("out_of_date_button");
  out_of_date_butto.value = "Thinking...";
  out_of_date_butto.disabled = true;
  
  
  jQuery.ajaxSync({
  	url: "/ajax/old_packages.html",
  	success: function(html)
  	{  
  	  var packs = JSON.parse(html);
  	  var i;
  	  var need_to_update = 0;
  	  
      // change the class name according to the package status
  	  for(i = 0; i < packs.length; i++){
  	    var row = document.getElementById("" + packs[i].Package);

  	    if(row != null){
  	      var pack_status = packs[i].status;
  	      
  	      row.className = pack_status;
  	      
  	      if(pack_status == "out_of_date")
      	    need_to_update++;
      	     
  	    }
  	  }
  	  
  	  var out_of_date_butto = document.getElementById("out_of_date_button");

  	  if(need_to_update > 0){
    	  out_of_date_butto.value = "Update " + need_to_update + " "+pluralize(need_to_update, "Package");
    	  out_of_date_butto.onclick = update_packs;
    	  out_of_date_butto.disabled = false;
  	  }
  	  else{
    	  out_of_date_butto.value = "All up to date";
    	  out_of_date_butto.onClick = "";
    	  //out_of_date_butto.disabled = true;
  	  }
  	  
  	  
  	  // stop spinning
  	}
  });
}

function update_packs()
{
  var out_of_date_butto = document.getElementById("out_of_date_button");
  out_of_date_butto.value = "Updating...";
  out_of_date_butto.disabled = true;

   jQuery.ajaxSync({
  	url: "/ajax/update_packs.html",
  	success: function(html)
  	{
      highlight_old_packages();      
  	}
  });
 
}

function pluralize(count, word)
{
  if(count > 1)
    return word + "s";
  else
    return word;
}
