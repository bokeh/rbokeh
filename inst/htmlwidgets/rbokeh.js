HTMLWidgets.widget({
  name: 'rbokeh',

  type: 'output',

  factory: function(el, width, height) {

    var bokeh_obj;

    return {
      renderValue: function(x) {
        // clear el for Shiny/dynamic contexts
        el.innerHTML = '';

        if(x.isJSON === true) {
          x.docs_json = JSON.parse(x.docs_json);
        }

        var refkey = Object.keys(x.docs_json)[0];
        var refs = x.docs_json[refkey].roots.references;

        if(x.debug === true) {
          console.log('docid: ' + x.docid)
          console.log('elementid: ' + x.elementid)
          console.log('modelid: ' + x.modelid)
          console.log(x.docs_json);
          console.log(JSON.stringify(x.docs_json));
        }

        // change 'nulls' in data to NaN
        function traverseObject(obj) {
          for(var key in obj) {
            if(obj[key].constructor === Object) {
              traverseObject(obj[key]);
            } else if(obj[key].constructor === Array) {
              for (var i = 0; i < obj[key].length; i++) {
                if(obj[key][i] === null)
                  obj[key][i] = NaN;
              }
            }
          }
        }

        for(var i = 0; i < refs.length; i++) {
          if(refs[i].type === 'ColumnDataSource')
            traverseObject(refs[i].attributes.data);
        }

        var dv1 = document.createElement('div');
        dv1.setAttribute('class', 'bk-root');

        var dv = document.createElement('div');

        dv.id = x.elementid;
        dv.setAttribute('class', 'plotdiv');
        dv1.appendChild(dv);
        el.appendChild(dv1);

        var render_items = [{
          'docid': x.docid,
          'elementid': x.elementid,
          'modelid': x.modelid
        }];

        if(x.debug !== true) {
          Bokeh.set_log_level('info');
        }

        bokeh_obj = Bokeh.embed.embed_items(x.docs_json, render_items);
      },

      resize: function(width, height) {
      },
      obj: bokeh_obj
    };
  }
});
