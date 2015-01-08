HTMLWidgets.widget({

  name: 'rbokeh',

  type: 'output',

  initialize: function(el, width, height) {
    return {

    }
  },

  renderValue: function(el, x, instance) {
    var modeltype = "Plot";
    Bokeh.logger.info("Realizing plot:")
    Bokeh.logger.info(" - modeltype: Plot");
    Bokeh.logger.info(" - modelid: " + x.modelid);
    Bokeh.logger.info(" - elementid: " + x.elementid);

    var dv = document.createElement('div');
    dv.id = x.elementid;
    dv.setAttribute("class", "plotdiv");
    el.appendChild(dv);
    console.log(x.all_models)
    Bokeh.load_models(x.all_models);
    var model = Bokeh.Collections(modeltype).get(x.modelid);
    var view = new model.default_view({model: model, el: '#' + x.elementid});
    Bokeh.index[x.modelid] = view;
  },

  resize: function(el, width, height, instance) {

  }

});
