package com.saada.springboot;

import org.springframework.web.bind.annotation.RestController;
import org.apache.tomcat.util.http.fileupload.FileUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import hex.genmodel.*;
import hex.genmodel.easy.EasyPredictModelWrapper;
import hex.genmodel.easy.RowData;
import hex.genmodel.easy.prediction.BinomialModelPrediction;
import hex.genmodel.easy.exception.PredictException;

@RestController
public class TestController {
    Logger logger = LoggerFactory.getLogger(this.getClass());
    static HashMap<String, EasyPredictModelWrapper> cachedModels = new HashMap<String, EasyPredictModelWrapper>();
    static final String MODEL_PATH = "models";
    @GetMapping("/models")
    public ResponseEntity<String[]> listModels() throws Exception {
        String[] filenames = Files
            .list(Paths.get(MODEL_PATH))
            .filter(Files::isRegularFile)
            .map(file -> file.getFileName().toString())
            .filter(filename -> filename.endsWith(".zip"))
            .map(filename -> filename.replace(".zip", ""))
            .map(filename -> {
                // warmup model
                try {
                    getCachedModel(filename);
                } catch (IOException e) {
                    logger.warn("caching failed", e.getMessage());
                }
                return filename;
            })
            .toArray(String[]::new);
        logger.info(filenames.toString());

        return new ResponseEntity<String[]>(filenames, HttpStatus.OK);
    }

    @PostMapping("/test")
    public Response index(@RequestBody Request req, @RequestHeader("x-model") String model) throws Exception {
        ObjectMapper oMapper = new ObjectMapper();
        oMapper.setVisibility(PropertyAccessor.FIELD, Visibility.ANY);

        RowData row = oMapper.convertValue(req, new TypeReference<RowData>() {
        });
        return execute(row, model);
    }

    private EasyPredictModelWrapper getCachedModel(String model) throws IOException {
        if (cachedModels.containsKey(model)) {
            logger.info("found cached model: " + model);
            return cachedModels.get(model);
        }
        URL mojoURL = new URL("file:///"+MODEL_PATH+"/"+ model + ".zip");
        MojoReaderBackend reader = MojoReaderBackendFactory.createReaderBackend(mojoURL,
                MojoReaderBackendFactory.CachingStrategy.MEMORY);
        MojoModel mojoModel = ModelMojoReader.readFrom(reader);
        EasyPredictModelWrapper modelWrapper = new EasyPredictModelWrapper(mojoModel);
        // add to cache
        cachedModels.put(model, modelWrapper);
        return modelWrapper;
    }

    public Response execute(RowData row, String model) throws IOException, PredictException {
        logger.info(row.toString());
        BinomialModelPrediction prediction = getCachedModel(model).predictBinomial(row);
        Response res = new Response();
        res.probability0 = prediction.classProbabilities[0];
        res.probability1 = prediction.classProbabilities[1];
        return res;
    }
}