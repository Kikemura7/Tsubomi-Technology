import Link from "../components/link";
import Layout from "../components/layout";
import { makeStyles, Grid, Paper } from "@material-ui/core";

const useStyles = makeStyles((theme) => ({
  root: {
    // flexGrow: 1,
  },
  paper: {
    padding: theme.spacing(2),
    textAlign: "center",
    // color: theme.palette.text.secondary,
  },
}));

export default function Home() {
  const classes = useStyles();

  return (
    <Layout>
      <Grid container item spacing={2}>
        <Grid item xs={12} sm={6}>
          <Link href="">
            <Paper className={classes.paper}>
              <h3>Documentation &rarr;</h3>
              <p>Find information about features</p>
            </Paper>
          </Link>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Link href="/try">
            <Paper className={classes.paper}>
              <h3>Try API&rarr;</h3>
              <p>Test the API</p>
            </Paper>
          </Link>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Link href="" className="card">
            <Paper className={classes.paper}>
              <h3>News &rarr;</h3>
              <p>Latest Updates</p>
            </Paper>
          </Link>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Link href="" className="card">
            <Paper className={classes.paper}>
              <h3>About &rarr;</h3>
              <p>Why we're working on this problem</p>
            </Paper>
          </Link>
        </Grid>
      </Grid>
    </Layout>
  );
}
