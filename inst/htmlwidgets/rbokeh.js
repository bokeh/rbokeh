HTMLWidgets.widget({

   name: 'rbokeh',

   type: 'output',

   initialize: function(el, width, height) {
      // var Plot = null;
      // return Plot;
      return {

      }
   },

   renderValue: function(el, x, instance) {

      // hack to deal with bad JSON for type="image"
      for (var i = 0; i < x.spec.length; i++) {
         if(x.spec[i].type == "image" || x.spec[i].type == "image_rgba")
            x.data[i].image = [x.data[i].image];
      }

      if(x.options.r_debug) {
         console.log(x);
         console.log(JSON.stringify(x.data));
         console.log(JSON.stringify(x.spec));
         console.log(JSON.stringify(x.options));
      }

      var Plot = Bokeh.Plotting.make_plot(x.spec, 
         x.data, x.options);
      
      Bokeh.Plotting.show(Plot, el);
   },

   resize: function(el, width, height, instance) {

   }

});
