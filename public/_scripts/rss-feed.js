    function LoadDynamicFeedControl() {
      var feeds = [
      	{
      	 title: 'Inside-R',
      	 url: 'http://www.inside-r.org/blogs/feed'
      	}/*,
      	{
      	 title: 'News',
      	 url: 'http://www.barretschloerke.com/R/news.xml'
      	},
      	{
      	 title: 'Changes',
      	 url: 'http://www.barretschloerke.com/R/changes.xml'
      	}*/];
      var options = {
        stacked : false,
        horizontal : true,
        title : ""
      }

      new GFdynamicFeedControl(feeds, 'feed-control', options);
    }
    // Load the feeds API and set the onload callback.
    google.load('feeds', '1');
    google.setOnLoadCallback(LoadDynamicFeedControl);
