# {
#   "attributes": {
#     "code": "\n        var data = source.get('data');\n        var start = range.get('start');\n        var end = range.get('end');\n        data['x'] = [start + (end - start) / 2];\n        data['width'] = [end - start];\n        source.trigger('change');\n    ",
#     "args": {
#       "source": {
#         "type": "ColumnDataSource",
#         "id": "ff93fc5a-b296-445e-a293-fd67e164bede"
#       },
#       "range": {
#         "type": "Range1d",
#         "id": "520ddf82-bfa6-433a-80f8-0c46e9bcf1e1"
#       }
#     }
#   },
#   "type": "CustomJS",
#   "id": "76737339-256b-49fe-b0e9-55ccd8d74c6c"
# }

# args should be a named list of refs to other things that code will reference

customjs_model <- function(type = "CustomJS", id, code, args) {
  res <- base_model_object(type, id)
  res$model$attributes$code <- code
  res$model$attributes$args <- args
  res
}


# {
#   "attributes": {
#     "callback": {
#       "type": "CustomJS",
#       "id": "76737339-256b-49fe-b0e9-55ccd8d74c6c"
#     },
#     "end": 100
#   },
#   "type": "Range1d",
#   "id": "520ddf82-bfa6-433a-80f8-0c46e9bcf1e1"
# }

# {
#   "attributes": {
#     "code": "\n        var data = source.get('data'); console.log(range);\n        var start = range.get('start');\n        var end = range.get('end');\n        data['y'] = [start + (end - start) / 2];\n        data['height'] = [end - start];\n        source.trigger('change');\n    ",
#     "args": {
#       "source": {
#         "type": "ColumnDataSource",
#         "id": "ff93fc5a-b296-445e-a293-fd67e164bede"
#       },
#       "range": {
#         "type": "Range1d",
#         "id": "b0dfac3a-0bb1-4f32-975e-de4162cd0a50"
#       }
#     }
#   },
#   "type": "CustomJS",
#   "id": "5799b231-29f5-4387-a794-6f9c20bd33eb"
# }


# {
#   "attributes": {
#     "callback": {
#       "type": "CustomJS",
#       "id": "5799b231-29f5-4387-a794-6f9c20bd33eb"
#     },
#     "end": 100
#   },
#   "type": "Range1d",
#   "id": "b0dfac3a-0bb1-4f32-975e-de4162cd0a50"
# }

