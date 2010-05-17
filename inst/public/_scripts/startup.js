$(document).ready(function(){
  $('#packages tr').quicksearch({
    position: 'before',
    attached: '#packages',
    labelText: 'Search: ',
    fixWidths: true
  });
})
