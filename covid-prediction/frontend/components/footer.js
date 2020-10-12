import Link from "./link";
import { Typography, Grid } from "@material-ui/core";

export default function Footer() {
  return (
    <Grid container direction="row" spacing={3}>
      <Grid
        container
        item
        direction="row"
        justify="center"
        alignItems="center"
        xs={12}
      >
        {[
          {name: "About", href: ""},
          {name: "Contact Us", href: ""},
          {name: "Privacy Policy", href: ""},
          {name: "Terms and Conditions", href: ""},
        ].map(i => <Grid key={i.name} item xs style={{width: '200px'}}>
          <Link color="inherit" href={i.href}>
            {i.name}
          </Link>
        </Grid>)}
      </Grid>
      <Grid container item xs justify="center" alignItems="center" xs={12}>
        <Typography variant="body2" color="textSecondary" align="center">
          {"Copyright © "}
          <Link color="inherit" href="/">
            {process.env.NEXT_PUBLIC_APP_NAME}
          </Link>{" "}
          {new Date().getFullYear()}
        </Typography>
      </Grid>
    </Grid>
  );
}
