package dotty.tools.dottydoc.staticsite;

import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.type.TypeReference;

import java.util.HashMap;
import java.io.ByteArrayInputStream;

public class Yaml {

  public static HashMap<String, Object> apply(String input)
      throws java.io.UnsupportedEncodingException, java.io.IOException {
    ByteArrayInputStream is = new ByteArrayInputStream(input.getBytes("UTF-8"));
    ObjectMapper mapper = new ObjectMapper(new YAMLFactory());

    TypeReference<HashMap<String, Object>> typeRef =
        new TypeReference<HashMap<String, Object>>() {};

    return mapper.readValue(is, typeRef);
  }
}
