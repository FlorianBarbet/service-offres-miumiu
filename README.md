# Service Offre

## Documentation

* [API documentation](/doc/api.md)

## Setting environment

The application need some environment variables.
Their is multiple ways to set them :

* Manually with CLI
example with the content of script :
```sh
 $ export APP_NAME_OFFRE="miumiu_offre"
 $ export POSTGRESQL_ADDON_HOST_OFFRE="127.0.0.1"
 $ export POSTGRESQL_ADDON_PORT_OFFRE="5432"
 $ export POSTGRESQL_ADDON_DB_OFFRE="offredb"
 $ export POSTGRESQL_ADDON_USER_OFFRE="miumiu"
 $ export POSTGRESQL_ADDON_PASSWORD_OFFRE="miumiu"
 $ export LEVEL="DEBUG"
 $ export FRONT_URI="http://localhost:8080"
 $ export AUTH_SERVICE_URI="http://localhost:3000"

```

For the nexts you should edit setEnv.sh script to match with your own environment data

* Script in the app repository ( need to reboot the terminal )
```sh
$ ./setEnv.sh 
```

* Put the script into init.d to keep variables at each reboot ( need to be sudoers )
```sh
$ sudo cp setEnv.sh /etc/init.d/set_service_offre_env_variables.sh
```

/!\ In production way you should change LEVEL variable content with 'INFO'

## Contributors

- @FlorianBarbet

- @anthonycouture