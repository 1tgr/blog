<% import urlparse %>
<div id="top_bar">
  <div class="ButtonBar">
      <div id="plugbanner"></div>
      <div id="blog_logo"></div>
      <h1><a href="${bf.config.blog.path}">
          <span id="blog_name">
            ${bf.config.blog.name}
          </span>
        </a>
      </h1>
  </div>
  <div id="search">    
    <form id="searchform" method="get" action="http://www.google.com/search">
      <input type="hidden" name="ie" value="UTF-8">
      <input type="hidden" name="oe" value="UTF-8">
      <input type="hidden" name="domains" value="${urlparse.urlparse(bf.config.site.url)[1]}">
      <input type="hidden" name="sitesearch" value="${urlparse.urlparse(bf.config.site.url)[1]}">
      <input name="q" id="q" size="20" value="search in blog..." onfocus="if(this.value==this.defaultValue) this.value='';" type="text">
    </form>
  </div>
</div>
