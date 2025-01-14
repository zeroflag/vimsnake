local equinox = require("equinox_bundle")
equinox.init()

table.insert(package.loaders, function(filename)
   if love.filesystem.getInfo(filename) then
      print("Loading: " .. filename);
      return function(...)
         return equinox.eval_file(love.filesystem.read(filename), {env=_G, filename=filename}, true), filename
      end
   end
end)

require("main.eqx")
