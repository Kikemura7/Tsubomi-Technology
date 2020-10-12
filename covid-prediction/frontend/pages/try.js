import { useState, useEffect } from "react";
import Select from "@material-ui/core/Select";
import ArrowUpwardIcon from "@material-ui/icons/ArrowUpward";
import Results from "../components/results";

import {
  FormControl,
  InputLabel,
  MenuItem,
  makeStyles,
  Button,
  Chip,
  Input,
  TextField,
  CircularProgress,
  FormControlLabel,
  Checkbox,
} from "@material-ui/core";
import Layout from "../components/layout";

const ITEM_HEIGHT = 48;
const ITEM_PADDING_TOP = 8;
const MenuProps = {
  PaperProps: {
    style: {
      maxHeight: ITEM_HEIGHT * 4.5 + ITEM_PADDING_TOP,
      width: 250,
    },
  },
};

const useStyles = makeStyles((theme) => ({
  formControl: {
    margin: theme.spacing(1),
    width: "100%",
  },
  chips: {
    display: "flex",
    flexWrap: "wrap",
  },
  chip: {
    margin: 2,
  },
  noLabel: {
    marginTop: theme.spacing(3),
  },
}));

const API_URL = "/api";
// {"systolicBP": 120, "diastolicBP": 60, "age": 50, "bun": 7, "ldh": 140, "pulseOx": 98, "cr": 0.9, "charlson_Score": 3, "troponin": 0.5, "ddimer": 1.5}
async function getModels() {}

export default function Try() {
  // the below variables have to start with lowercase character for API to work
  const [formFields, setFormFields] = useState({
    systolicBP: 120,
    diastolicBP: 60,
    age: 50,
    bun: 7,
    ldh: 140,
    pulseOx: 98,
    glucose: 100,
    rr: 10,
    troponin: 0.5,
    ddimer: 1.5,
  });

  const [models, setModels] = useState({
    list: [],
    selected: ["GBM"],
  });

  const [isSelectBoxLoading, setSelectBoxLoading] = useState(false);
  const [isFormSubmitLoading, setFormSubmitLoading] = useState(false);
  const [disclaimerChecked, setDisclaimerChecked] = useState(false);

  const fetchModels = async () => {
    setSelectBoxLoading(true);
    const res = await fetch(API_URL, { method: "GET" });
    const json = await res.json();
    if (json.type === "success") {
      setModels({ ...models, list: json.message });
      setSelectBoxLoading(false);
    } else {
      console.error(json);
    }
  };

  useEffect(() => {
    fetchModels();
  }, []);

  const [response, setResponse] = useState({
    type: "success",
    message: "",
  });

  const handleChange = (e) =>
    setFormFields({ ...formFields, [e.target.name]: e.target.value });

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      setFormSubmitLoading(true);
      const promises = await Promise.all(
        models.selected.map((model) =>
          fetch(API_URL, {
            method: "POST",
            body: JSON.stringify(formFields),
            headers: {
              "Content-Type": "application/json",
              "x-model": model,
            },
          })
        )
      );

      const responses = [];
      for (let i = 0; i < promises.length; i++) {
        const json = await promises[i].json();
        if (json.type === "success") {
          responses.push({ model: models.selected[i], results: json.message });
        } else {
          setResponse({
            type: "error",
            message: JSON.stringify(json),
          });
        }
      }
      setResponse({
        type: "success",
        message: JSON.stringify(responses, null, 2),
      });
    } catch (e) {
      console.log("An error occurred", e);
      setResponse({
        type: "error",
        message: "An error occured while submitting the form",
      });
    }
    setFormSubmitLoading(false);
  };

  let output = null;
  if (response.type === "error") {
    output = <div style={{ color: "red" }}>{response.message}</div>;
  } else if (response.message) {
    output = <Results payload={response.message} />;
  }
  const classes = useStyles();
  return (
    <Layout>
      <h1>Prediction API</h1>
      <p>
        Enter patient metrics below and the API will attempt to predict the
        patient's chance of survival
      </p>
      <form action="{API_URL}" method="post" onSubmit={handleSubmit}>
        {Object.keys(formFields).map((f) => (
          <TextField
            key={f}
            id={"input__" + f}
            name={f}
            label={f}
            value={formFields[f]}
            onChange={handleChange}
            style={{ margin: "5px", width: "auto" }}
            // helperText="Enter a number"
          />
        ))}
        <br />
        <FormControl className={classes.formControl}>
          <InputLabel id="models-label">Models</InputLabel>
          <Select
            labelId="models-label"
            id="models-chip"
            disabled={isSelectBoxLoading}
            multiple
            value={models.selected}
            onChange={(e) => setModels({ ...models, selected: e.target.value })}
            input={<Input id="select-multiple-models" />}
            renderValue={(selected) => (
              <div className={classes.chips}>
                {models.selected.map((value) => (
                  <Chip
                    key={"chip__" + value}
                    label={value}
                    color="secondary"
                    className={classes.chip}
                  />
                ))}
              </div>
            )}
            MenuProps={MenuProps}
          >
            {models.list.map((model) => (
              <MenuItem key={"item__" + model} value={model} color="secondary">
                {model}
              </MenuItem>
            ))}
          </Select>
          {isSelectBoxLoading ? <CircularProgress color="secondary" /> : null}
        </FormControl>
        <FormControlLabel
          control={
            <Checkbox
              required
              checked={disclaimerChecked}
              onChange={() => setDisclaimerChecked(!disclaimerChecked)}
              name="disclaimerChecked"
              color="secondary"
            />
          }
          label="I understand that this app is only for educational purpose and not for diagnosis."
        />
        <br />
        <Button
          type="submit"
          size="large"
          startIcon={<ArrowUpwardIcon />}
          variant="contained"
          color="primary"
          disabled={isFormSubmitLoading}
        >
          Submit
        </Button>
        {isFormSubmitLoading ? <CircularProgress color="primary" /> : null}
        <div>{output}</div>
      </form>
    </Layout>
  );
}
